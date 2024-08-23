import LanguageServerProtocol
import PackageModel
import PackageModelSyntax
import SwiftRefactor
import SwiftSyntax
import SwiftSyntaxBuilder
import SwiftBasicFormat
import SwiftParser
@_spi(FixItApplier) import SwiftIDEUtils

extension TextEdit {
    fileprivate init(_ edit: SourceEdit, scope: SyntaxCodeActionScope) {
        self.init(
            range: scope.snapshot.absolutePositionRange(of: edit.range),
            newText: edit.replacement
        )
    }
    
    fileprivate init(_ edit: SourceEdit, snapshot: DocumentSnapshot) {
        self.init(
            range: snapshot.absolutePositionRange(of: edit.range),
            newText: edit.replacement
        )
    }
}

fileprivate struct MacroDeclaration {
    
    let parameters: FunctionParameterListSyntax
    let module: String
    let type: String
    let roles: String
    
    init?(_ node: MacroDeclSyntax?) {
        guard let node else {
            return nil
        }
        let parameters = node.signature.parameterClause.parameters
        guard let expansion = node.definition?.value.as(MacroExpansionExprSyntax.self) else {
            return nil
        }
        guard let module = expansion.module else {
            return nil
        }
        guard let type = expansion.type else {
            return nil
        }
        let attributes = node.attributes.compactMap({ $0.as(AttributeSyntax.self) })
        let roles = attributes.compactMap { $0.arguments?.as(LabeledExprListSyntax.self)?.trimmedDescription }.joined(separator: ",")
        
        self.parameters = parameters
        self.module = module
        self.type = type
        self.roles = roles
    }
    
}

struct AddMacroImplementation: SyntaxCodeActionProvider {
    
    static func codeActions(in scope: SyntaxCodeActionScope) -> [CodeAction] {
        
        var actions = [CodeAction]()
        
        var documentChanges: [WorkspaceEditDocumentChange] = []
        
        guard let node = scope.innermostNodeContainingRange?.findEnclosingMacro() else {
            return []
        }
        
        guard scope.request.range.upperBound >= scope.snapshot.position(of: node.positionAfterSkippingLeadingTrivia) else {
            return []
        }
        
        guard let macro = MacroDeclaration(node) else {
            return []
        }
        
        let module = macro.module
        let type = macro.type
        let roles = macro.roles
        let parameters = macro.parameters

        guard let root = scope.findSourcesFolder() else {
            return []
        }
                
        let uri = DocumentURI(root.appending(path: module).appending(path: "\(type).swift"))
        
        let create = CreateFile(uri: uri)
        
        var macroImplementationText = makeImplementation(for: macro)
                
        let edit = TextEdit(
            SourceEdit(
                range: node.positionAfterSkippingLeadingTrivia..<node.positionAfterSkippingLeadingTrivia,
                replacement: macroImplementationText
            ),
            scope: scope
        )

        documentChanges.append(.createFile(create))
        documentChanges.append(.textDocumentEdit(TextDocumentEdit(textDocument: .init(uri, version: scope.snapshot.version), edits: [.textEdit(edit)])))
        
        if let plugin = scope.findMacroPlugin(module: module), let snapshot = try? DocumentSnapshot(withContentsFromDisk: plugin.url, language: .swift) {
            let updatedPlugin = addProvidingMacro(type, plugin: plugin.node).formatted()
            let edit = TextEdit(SourceEdit(range: plugin.node.range, replacement: updatedPlugin.description), snapshot: snapshot)
            documentChanges.append(.textDocumentEdit(TextDocumentEdit(textDocument: .init(DocumentURI(plugin.url), version: snapshot.version), edits: [.textEdit(edit)])))
        } else {
            
            let pluginText = """
            import SwiftCompilerPlugin
            import SwiftSyntaxMacros
            
            @main
            struct \(module)Plugin: CompilerPlugin {
                let providingMacros: [Macro.Type] = [
                    \(type).self
                ]
            }
            """
            
            let pluginURL = root.appending(path: module).appending(path: "\(module)Plugin.swift")
            
            let uri = DocumentURI(pluginURL)
            
            let create = CreateFile(uri: uri)
            
            documentChanges.append(.createFile(create))
            
            let edit = TextEdit(range: .init(Position(line: 0, utf16index: 0)), newText: pluginText)
            
            documentChanges.append(.textDocumentEdit(TextDocumentEdit(textDocument: .init(uri, version: scope.snapshot.version), edits: [.textEdit(edit)])))
            
        }
        
        if let manifest = scope.findPackageManifest(), let snapshot = try? DocumentSnapshot(withContentsFromDisk: manifest.url, language: .swift) {
            
            if !manifest.node.hasMacroTarget(named: module), let target = scope.target {
                
                do {
                    let addTarget = try AddTarget.addTarget(TargetDescription(name: module, type: .macro), to: manifest.file)
                    
                    let manifestWithMacroTarget = manifest.file.apply(addTarget.manifestEdits)
                    
                    let addTargetDependency = try AddTargetDependency.addTargetDependency(.byName(name: module, condition: nil), targetName: target.name, to: manifestWithMacroTarget)
                    
                    let finalManifest = manifestWithMacroTarget.apply(addTargetDependency.manifestEdits)
                    
                    let finalManifestEdit = SourceEdit(range: manifest.file.range, replacement: finalManifest.description)
                    
                    var finalPackageEditResult = addTarget
                    
                    finalPackageEditResult.manifestEdits = [finalManifestEdit]
                    
                    let packageWorkspaceEdit = finalPackageEditResult.asWorkspaceEdit(snapshot: snapshot)
                    
                    finalPackageEditResult.asWorkspaceEdit(snapshot: snapshot).changes?.values.flatMap({ $0 }).forEach { change in
                        documentChanges.append(
                            .textDocumentEdit(
                                TextDocumentEdit(
                                    textDocument: .init(snapshot.uri, version: snapshot.version),
                                    edits: [.textEdit(change)]
                                )
                            )
                        )
                    }
                } catch {
                    
                }
                
            }
            
        }
        
        actions.append(
            CodeAction(
                title: "Add Macro Implementation",
                kind: .refactorInline,
                edit: WorkspaceEdit(
                    changes: nil,
                    documentChanges: documentChanges
                )
            )
        )
        
        return actions
    }
    
    private static func makeImplementation(for macro: MacroDeclaration) -> String {
        var implementation = """
        import SwiftSyntax
        import SwiftSyntaxMacros
        
        public struct \(macro.type) { }
        
        """
        implementation.append(contentsOf: makeMacroExtensions(type: macro.type, parameters: macro.parameters, roles: macro.roles))
        return implementation

    }

    
    private static func makeMacroExtensions(type: String, parameters: FunctionParameterListSyntax, roles: String) -> String {
        var extensions = ""
        
        let roles = roles.components(separatedBy: ",")
        
        if roles.contains("expression") {
            let decl = makeExpressionMacroExtension(type)
            extensions.append(decl.formatted().description)
        }
        
        if roles.contains("declaration") {
            let decl = makeDeclarationMacroExtension(type)
            extensions.append(decl.formatted().description)
        }
        
        if roles.contains("accessor") || roles.contains("member") || roles.contains("memberAttribute") || roles.contains("peer") {
                        
            let body = makeBody(parameters)
            
            if roles.contains("accessor") {
                let decl = makeAccessorMacroExtension(type, body: body)
                extensions.append(decl.formatted().description)
            }
            
            if roles.contains("member") {
                let decl = makeMemberMacroExtension(type, body: body)
                extensions.append(decl.formatted().description)
            }
            
            if roles.contains("memberAttribute") {
                let decl = makeMemberAttributeMacroExtension(type, body: body)
                extensions.append(decl.formatted().description)
            }
            
            if roles.contains("peer") {
                let decl = makePeerMacroExtension(type, body: body)
                extensions.append(decl.formatted().description)
            }
            
            if roles.contains("extension") {
                let decl = makePeerMacroExtension(type, body: body)
                extensions.append(decl.formatted().description)
            }
        }
        
        return extensions
    }
    
    private static func addProvidingMacro(_ macro: String, plugin: StructDeclSyntax) -> StructDeclSyntax {
        guard let providingMacros = plugin.memberBlock.members.first(where: { $0.description.contains("providingMacros") })?.decl.as(VariableDeclSyntax.self) else {
            return plugin
        }
        let macro: ExprSyntax = "\(raw: macro).self"
        if let array = providingMacros.bindings.first?.initializer?.value.as(ArrayExprSyntax.self) {
            let updatedArray = array.appending(element: macro)
            return plugin.replacingChild(Syntax(array), with: Syntax(updatedArray))
        }
        return plugin
    }
    
    private static func makeBody(_ parameters: FunctionParameterListSyntax) -> String {
        let bindings = makeParametersBindings(parameters)
        
        let body = if bindings.isEmpty {
            "return []"
        } else {
            """
            \(bindings)
            return []
            """
        }
        
        return body
    }
    
    private static func makeParametersBindings(_ parameters: FunctionParameterListSyntax) -> String {
        if parameters.isEmpty {
            return ""
        }
        var bindings = """
        guard let arguments = node.arguments?.as(LabeledExprListSyntax.self) else { return [] }
        """
        for (offset, parameter) in parameters.enumerated() {
            let name = parameter.secondName ?? parameter.firstName
            bindings.append("\n")
            bindings.append("let \(name) = arguments[arguments.index(at: \(offset)]")
        }
        return bindings
    }
    
    private static func makeExpressionMacroExtension(_ name: String) -> DeclSyntax {
        #"""
        
        public extension \#(raw: name): ExpressionMacro {
            public static func expansion(
                of node: some FreestandingMacroExpansionSyntax,
                in context: some MacroExpansionContext
            ) -> ExprSyntax {
                return ""
            }
        }
        
        """#
    }
    
    private static func makeDeclarationMacroExtension(_ name: String) -> DeclSyntax {
        #"""
        
        public extension \#(raw: name): DeclarationMacro {
            public static func expansion(
                of node: some FreestandingMacroExpansionSyntax,
                in context: some MacroExpansionContext
            ) throws -> [DeclSyntax] {
                return []
            }
        }
        
        """#
    }
    
    private static func makeAccessorMacroExtension(_ name: String, body: String) -> DeclSyntax {
        #"""
        
        public extension \#(raw: name): AccessorMacro {
            public static func expansion(
                of node: AttributeSyntax,
                providingAccessorsOf declaration: some DeclSyntaxProtocol,
                in context: some MacroExpansionContext
            ) throws -> [AccessorDeclSyntax] {
                \#(raw: body)
            }
        }
        
        """#
    }
    
    private static func makeMemberMacroExtension(_ name: String, body: String) -> DeclSyntax {
        #"""
        
        public extension \#(raw: name): MemberMacro {
            public static func expansion(
                of node: AttributeSyntax,
                providingMembersOf declaration: some DeclGroupSyntax,
                in context: some MacroExpansionContext
            ) throws -> [DeclSyntax] {
                \#(raw: body)
            }
        }
        
        """#
    }
    
    private static func makeMemberAttributeMacroExtension(_ name: String, body: String) -> DeclSyntax {
        #"""
        
        public extension \#(raw: name): MemberAttributeMacro {
            public static func expansion(
                of node: AttributeSyntax,
                attachedTo declaration: some DeclGroupSyntax,
                providingAttributesFor member: some DeclSyntaxProtocol,
                in context: some MacroExpansionContext
            ) throws -> [AttributeSyntax] {
                \#(raw: body)
            }
        }
        
        """#
    }
    
    private static func makePeerMacroExtension(_ name: String, body: String) -> DeclSyntax {
        #"""
        
        public extension \#(raw: name): PeerMacro {
            public static func expansion(
                of node: AttributeSyntax,
                providingPeersOf declaration: some DeclSyntaxProtocol,
                in context: some MacroExpansionContext
            ) throws -> [DeclSyntax] {
                \#(raw: body)
            }
        }
        
        """#
    }
    
    private static func makeExtensionMacroExtension(_ name: String, parameters: String) -> DeclSyntax {
        #"""
        
        public extension \#(raw: name): ExtensionMacro {
            static func expansion(
                of node: AttributeSyntax,
                attachedTo declaration: some DeclGroupSyntax,
                providingExtensionsOf type: some TypeSyntaxProtocol,
                conformingTo protocols: [TypeSyntax],
                in context: some MacroExpansionContext
            ) throws -> [ExtensionDeclSyntax]
                return []
            }
        }
        
        """#
    }
    
}

// MARK: Utils

fileprivate extension FunctionCallExprSyntax {
    
    var targets: [TargetDescription]? {
        if let targets = arguments.first(labeled: "targets")?.expression.as(ArrayExprSyntax.self) {
            targets.elements.compactMap {
                TargetDescription($0.expression)
            }
        } else {
            nil
        }
    }
    
    func target(named targetName: String) -> TargetDescription? {
        if let targets = arguments.first(labeled: "targets")?.expression.as(ArrayExprSyntax.self) {
            targets.elements.compactMap {
                TargetDescription($0.expression)
            }.first {
                $0.name == targetName || $0.path?.contains(targetName) == true
            }
        } else {
            nil
        }
    }
    
    func hasMacroTarget(named targetName: String) -> Bool {
        if let targets = arguments.first(labeled: "targets")?.expression.as(ArrayExprSyntax.self) {
            return targets.elements.contains {
                if let target = TargetDescription($0.expression), target.type == .macro, target.name == targetName {
                    return true
                } else {
                    return false
                }
            }
        } else {
            return false
        }
    }
    
    func macroTarget(named name: String) -> ExprSyntax? {
        if let targets = arguments.first(labeled: "targets")?.expression.as(ArrayExprSyntax.self) {
            let target = targets.elements.first {
                if let target = TargetDescription($0.expression), target.type == .macro, target.name == name {
                    return true
                } else {
                    return false
                }
            }
            return target?.expression
        } else {
            return nil
        }
    }
}

extension SourceFileSyntax {
    
    func apply(_ manifestEdits: [SourceEdit]) -> SourceFileSyntax {
        var parser = Parser(FixItApplier.apply(edits: manifestEdits, to: self))
        return SourceFileSyntax.parse(from: &parser)
    }
    
}

fileprivate extension SyntaxCodeActionScope {
    
    var target: TargetDescription? {
        guard let fileURL = snapshot.uri.fileURL else {
            return nil
        }
        guard let manifest = findPackageManifest() else {
            return nil
        }
        guard let targets = manifest.node.targets else {
            return nil
        }
        return targets.first {
            fileURL.absoluteString.contains(($0.path ?? "Sources/\($0.name)"))
        }
    }
    
    var targetName: String? {
        self.snapshot.uri.fileURL?.deletingLastPathComponent().lastPathComponent
    }
    
    func findPackageManifest() -> (url: URL, file: SourceFileSyntax, node: FunctionCallExprSyntax)? {
        if let url = findSourcesFolder()?.deletingLastPathComponent().appending(component: "Package.swift") {
            do {
                let input = try String(contentsOf: url)
                var parser = Parser(input)
                let file = SourceFileSyntax.parse(from: &parser)
                if let node = FunctionCallExprSyntax.findFirst(in: file, matching: { $0.calledExpression.description == "Package" }) {
                    return (url, file, node)
                }
            } catch {
                return nil
            }
        }
        return nil
    }
    
    func findMacroPlugin(module: String) -> (url: URL, node: StructDeclSyntax)? {
        if let pluginURL = findSourcesFolder()?.appending(components: module, "\(module)Plugin.swift") {
            do {
                let input = try String(contentsOf: pluginURL)
                var parser = Parser(input)
                if let node = StructDeclSyntax.findFirst(in: SourceFileSyntax.parse(from: &parser), matching: { $0.name.description == "\(module)Plugin" }) {
                    return (pluginURL, node)
                }
            } catch {
                return nil
            }
        }
        return nil
    }
    
    func findSourcesFolder() -> URL? {
        if let fileURL = self.snapshot.uri.fileURL, let range = fileURL.absoluteString.range(of: "/Sources") {
            return URL(string: String(fileURL.absoluteString[..<range.upperBound]))
        }
        return nil
    }
    
}

fileprivate extension SyntaxProtocol {
    func findEnclosingMacro() -> MacroDeclSyntax? {
        var current = Syntax(self)
        while true {
            if let call = current.as(MacroDeclSyntax.self) {
                return call
            }
            
            if let parent = current.parent {
                current = parent
                continue
            }
            
            return nil
        }
    }
}

fileprivate extension MacroExpansionExprSyntax {
    var module: String? {
        arguments.first(labeled: "module")?.expression.as(StringLiteralExprSyntax.self)?.segments.trimmedDescription
    }
    var type: String? {
        arguments.first(labeled: "type")?.expression.as(StringLiteralExprSyntax.self)?.segments.trimmedDescription
    }
}

fileprivate extension LabeledExprListSyntax {
    func first(labeled label: String) -> LabeledExprSyntax? {
        self.first(where: { $0.label?.text == label })
    }
}

private class FirstNodeFinder<Node: SyntaxProtocol>: SyntaxAnyVisitor {
    var predicate: (Node) -> Bool
    var found: Node? = nil
    
    init(predicate: @escaping (Node) -> Bool) {
        self.predicate = predicate
        super.init(viewMode: .sourceAccurate)
    }
    
    override func visitAny(_ node: Syntax) -> SyntaxVisitorContinueKind {
        if found != nil {
            return .skipChildren
        }
        
        if let matchedNode = node.as(Node.self), predicate(matchedNode) {
            found = matchedNode
            return .skipChildren
        }
        
        return .visitChildren
    }
}

extension SyntaxProtocol {
    static func findFirst(
        in node: some SyntaxProtocol,
        matching predicate: (Self) -> Bool = { _ in true }
    ) -> Self? {
        withoutActuallyEscaping(predicate) {
            let visitor = FirstNodeFinder<Self>(predicate: $0)
            visitor.walk(node)
            return visitor.found
        }
    }
}

fileprivate class ReplacingRewriter: SyntaxRewriter {
    let childNode: Syntax
    let newChildNode: Syntax
    
    init(childNode: Syntax, newChildNode: Syntax) {
        self.childNode = childNode
        self.newChildNode = newChildNode
        super.init()
    }
    
    override func visitAny(_ node: Syntax) -> Syntax? {
        if node == childNode {
            return newChildNode
        }
        
        return nil
    }
}

fileprivate extension SyntaxProtocol {
    func replacingChild(_ childNode: Syntax, with newChildNode: Syntax) -> Self {
        ReplacingRewriter(
            childNode: childNode,
            newChildNode: newChildNode
        )
        .rewrite(self)
        .cast(Self.self)
    }
}

//extension SyntaxCollection where Self: WithTrailingCommaSyntax {
//    func appending(element: Self.Element) -> Self {
//        return self
//    }
//}

extension ArrayExprSyntax {
    /// Produce a new array literal expression that appends the given
    /// element, while trying to maintain similar indentation.
    func appending(element: ExprSyntax) -> ArrayExprSyntax {
        var elements = self.elements
        
        let commaToken = TokenSyntax.commaToken()
        
        // If there are already elements, tack it on.
        let leadingTrivia: Trivia
        let trailingTrivia: Trivia
        let leftSquareTrailingTrivia: Trivia
        if let last = elements.last {
            // The leading trivia of the new element should match that of the
            // last element.
            leadingTrivia = last.leadingTrivia.onlyLastLine()
            
            // Add a trailing comma to the last element if it isn't already
            // there.
            if last.trailingComma == nil {
                var newElements = Array(elements)
                newElements[newElements.count - 1].trailingComma = commaToken
                newElements[newElements.count - 1].expression.trailingTrivia = Trivia()
                newElements[newElements.count - 1].trailingTrivia = last.trailingTrivia
                elements = ArrayElementListSyntax(newElements)
            }
            
            trailingTrivia = Trivia()
            leftSquareTrailingTrivia = leftSquare.trailingTrivia
        } else {
            leadingTrivia = Trivia()
            trailingTrivia = Trivia()
            if leftSquare.trailingTrivia.contains(where: \.isNewline) {
                leftSquareTrailingTrivia = leftSquare.trailingTrivia
            } else {
                leftSquareTrailingTrivia = Trivia()
            }
        }
        
        elements.append(
            ArrayElementSyntax(
                expression: element.with(\.leadingTrivia, leadingTrivia),
                trailingComma: commaToken.with(\.trailingTrivia, trailingTrivia)
            )
        )
        
        let newLeftSquare = leftSquare.with(
            \.trailingTrivia,
             leftSquareTrailingTrivia
        )
        
        return with(\.elements, elements).with(\.leftSquare, newLeftSquare)
    }
}

extension Trivia {
    /// Produce trivia from the last newline to the end, dropping anything
    /// prior to that.
    func onlyLastLine() -> Trivia {
        guard let lastNewline = pieces.lastIndex(where: { $0.isNewline }) else {
            return self
        }
        
        return Trivia(pieces: pieces[lastNewline...])
    }
}

extension TargetDescription {
    
    fileprivate init?(_ node: ExprSyntax) {
        guard let call = node.as(FunctionCallExprSyntax.self) else {
            return nil
        }
        
        guard let kindName = call.calledExpression.as(MemberAccessExprSyntax.self)?.declName.baseName.text else {
            return nil
        }
        
        let kind = TargetDescription.TargetKind(rawValue: kindName) ?? .regular
        
        guard let name = call.arguments.first(labeled: "name")?.expression.as(StringLiteralExprSyntax.self)?.segments.trimmedDescription else {
            return nil
        }
        
        let path = call.arguments.first(labeled: "path")?.expression.as(StringLiteralExprSyntax.self)?.segments.trimmedDescription
        
        let dependencies = call.arguments.first(labeled: "dependencies")?.expression.as(ArrayExprSyntax.self)?.elements.compactMap {
            TargetDescription.Dependency($0.expression)
        }
        
        try? self.init(name: name, dependencies: dependencies ?? [], path: path, type: kind)
    }
    
}

extension TargetDescription.Dependency {
    
    fileprivate init?(_ node: ExprSyntax) {
        if let name = node.as(StringLiteralExprSyntax.self)?.segments.description {
            self = .byName(name: name, condition: nil)
        } else {
            return nil
        }
    }
    
    fileprivate var name: String {
        switch self {
            case .target(let name, _):
                return name
            case .product(let name, _, _, _):
                return name
            case .byName(let name, _):
                return name
        }
    }
    
}
