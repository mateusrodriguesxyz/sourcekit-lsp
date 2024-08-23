import LanguageServerProtocol
import PackageModel
import PackageModelSyntax
import SwiftRefactor
import SwiftSyntax
import SwiftSyntaxBuilder

fileprivate enum MacroRole: String {
    case expression
    case declaration
    case accessor
    case member
    case memberAttribute
    case peer
    case `extension`
}

struct Person: Codable {
    let name: String
    let age: Int
}

extension TextEdit {
    fileprivate init(_ edit: SourceEdit, scope: SyntaxCodeActionScope) {
        self.init(
            range: scope.snapshot.absolutePositionRange(of: edit.range),
            newText: edit.replacement
        )
    }
}

struct AddMacroImplementation: SyntaxCodeActionProvider {
    
    static func codeActions(in scope: SyntaxCodeActionScope) -> [CodeAction] {
        
        
        
        guard let macro = scope.innermostNodeContainingRange?.findEnclosingMacro() else {
            return []
        }
        
        let parameters = macro.signature.parameterClause.parameters
        
        guard let expansion = macro.definition?.value.as(MacroExpansionExprSyntax.self) else {
            return []
        }
        
        guard let module = expansion.arguments.labeled("module")?.expression.as(StringLiteralExprSyntax.self)?.segments.trimmedDescription else {
            return []
        }
        
        guard let type = expansion.arguments.labeled("type")?.expression.as(StringLiteralExprSyntax.self)?.segments.trimmedDescription else {
            return []
        }
        
        if !parameters.isEmpty {
            let _ = makeParametersBindings(parameters)
        }
        
        guard scope.request.range.upperBound >= scope.snapshot.position(of: macro.positionAfterSkippingLeadingTrivia) else {
            return []
        }
                        
        let attributes = macro.attributes.compactMap({ $0.as(AttributeSyntax.self) })
                
        let roles = attributes.compactMap { $0.arguments?.as(LabeledExprListSyntax.self)?.trimmedDescription }.joined(separator: ",")
                
        guard let root = scope.snapshot.uri.fileURL?.deletingLastPathComponent().deletingLastPathComponent() else {
            return []
        }
        
        guard let directory = scope.snapshot.uri.fileURL?.deletingLastPathComponent().lastPathComponent else {
            return []
        }
        
        let url = root.appending(path: directory + "Macros").appending(path: "\(type).swift")
           
        let uri = DocumentURI(url)
        
        
        let create = CreateFile(uri: uri)
        
        var replacement = #"""
        import SwiftSyntax
        import SwiftSyntaxMacros
        
        public struct \#(type) { }
        
        """#
        
        if roles.contains("expression") {
            replacement.append(makeExpressionMacroExtension(type))
        }
        
        if roles.contains("declaration") {
            replacement.append(makeDeclarationMacroExtension(type))
        }
        
        if roles.contains("accessor") {
            replacement.append(makeAccessorMacroExtension(type))
        }
        
        if roles.contains("member") {
            replacement.append(makeMemberMacroExtension(type))
        }
        
        if roles.contains("memberAttribute") {
            replacement.append(makeMemberAttributeMacroExtension(type))
        }
        
        if roles.contains("peer") {
            replacement.append(makePeerMacroExtension(type))
        }
        
        let plugin = """
        @main
        struct MyMacroPlugin: CompilerPlugin {
            let providingMacros: [Macro.Type] = [
                \(type).self,
            ]
        }
        """
        
        let _edit = SourceEdit(
            range: macro.positionAfterSkippingLeadingTrivia..<macro.positionAfterSkippingLeadingTrivia,
            replacement: replacement
        )
        
        let edit = TextEdit(_edit, scope: scope)
        
        let _edit2 = SourceEdit(
            range: macro.positionAfterSkippingLeadingTrivia..<macro.positionAfterSkippingLeadingTrivia,
            replacement: "// \(roles) \n"
        )
        
        let edit2 = TextEdit(_edit2, scope: scope)
        
        return [
            CodeAction(
              title: "Add Macro Implementation",
              kind: .refactorInline,
              edit: WorkspaceEdit(
                changes: [scope.snapshot.uri : [edit]]
//                documentChanges: [
//                    .createFile(create),
//                    .textDocumentEdit(TextDocumentEdit(textDocument: .init(uri, version: scope.snapshot.version), edits: [.textEdit(edit)]))]
              )
            )
        ]
    }
    
    private static func addProvidingMacro(_ macro: String, plugin: StructDeclSyntax) {
        for member in plugin.memberBlock.members {
            if member.description.contains("providingMacros"), let providingMacros = member.decl.as(VariableDeclSyntax.self) {
                providingMacros.description.lastin
            }
        }
    }
    
    private static func makeParametersBindings(_ parameters: FunctionParameterListSyntax) -> String {
        var bindings = """
        guard let arguments = node.arguments?.as(LabeledExprListSyntax.self) else {
            return []
        }
        """
        for (offset, parameter) in parameters.enumerated() {
            let name = parameter.secondName ?? parameter.firstName
            bindings.append("\n")
            bindings.append("let \(name) = arguments[arguments.index(at: \(offset)]")
        }
    }
    
    private static func makeExpressionMacroExtension(_ name: String) -> String {
        #"""
        
        public extension \#(name): ExpressionMacro {
            public static func expansion(
                of node: some FreestandingMacroExpansionSyntax,
                in context: some MacroExpansionContext
            ) -> ExprSyntax {
                return ""
            }
        }
        
        """#
    }
    
    private static func makeDeclarationMacroExtension(_ name: String) -> String {
        #"""
        
        public extension \#(name): DeclarationMacro {
            public static func expansion(
                of node: some FreestandingMacroExpansionSyntax,
                in context: some MacroExpansionContext
            ) throws -> [DeclSyntax] {
                return []
            }
        }
        
        """#
    }
    
    private static func makeAccessorMacroExtension(_ name: String) -> String {
        #"""
        
        public extension \#(name): AccessorMacro {
            public static func expansion(
                of node: AttributeSyntax,
                providingAccessorsOf declaration: some DeclSyntaxProtocol,
                in context: some MacroExpansionContext
            ) throws -> [AccessorDeclSyntax] {
                return []
            }
        }
        
        """#
    }
    
    private static func makeMemberMacroExtension(_ name: String) -> String {
        #"""
        
        public extension \#(name): MemberMacro {
            public static func expansion(
                of node: AttributeSyntax,
                providingMembersOf declaration: some DeclGroupSyntax,
                in context: some MacroExpansionContext
            ) throws -> [DeclSyntax] {
                return []
            }
        }
        
        """#
    }
    
    private static func makeMemberAttributeMacroExtension(_ name: String) -> String {
        #"""
        
        public extension \#(name): MemberAttributeMacro {
            public static func expansion(
                of node: AttributeSyntax,
                attachedTo declaration: some DeclGroupSyntax,
                providingAttributesFor member: some DeclSyntaxProtocol,
                in context: some MacroExpansionContext
            ) throws -> [AttributeSyntax] {
                return []
            }
        }
        
        """#
    }
    
    private static func makePeerMacroExtension(_ name: String) -> String {
        #"""
        
        public extension \#(name): PeerMacro {
            public static func expansion(
                of node: AttributeSyntax,
                providingPeersOf declaration: some DeclSyntaxProtocol,
                in context: some MacroExpansionContext
            ) throws -> [DeclSyntax] {
                return []
            }
        }
        
        """#
    }
    
    private static func makeExtensionMacroExtension(_ name: String) -> String {
        #"""
        
        public extension \#(name): ExtensionMacro {
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

fileprivate extension SyntaxCodeActionScope {
    func findMacroPlugin() -> (URL, Syntax)? {
        nil
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

fileprivate extension LabeledExprListSyntax {
    func labeled(_ label: String) -> LabeledExprSyntax? {
        self.first(where: { $0.label?.text == label })
    }
}
