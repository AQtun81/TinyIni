using System.Collections.Immutable;
using System.Text;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;

[Generator]
public class TinyIniGenerator : IIncrementalGenerator
{
    private static IndentationHelper ih = new();
    
    private struct IndentationHelper
    {
        private static readonly StringBuilder sb = new(64);
        public ushort Depth = 0;

        public string Pad
        {
            get
            {
                sb.Clear();
                for (int i = 0; i < Depth; i++)
                {
                    sb.Append("    ");
                }
                return sb.ToString();
            }
        }
        
        public string Open
        {
            get
            {
                string result = $"{Pad}{{";
                Depth += 1;
                return result;
            }
        }
        
        
        public string Close
        {
            get
            {
                Depth -= 1;
                return $"{Pad}}}";
            }
        }

        public IndentationHelper() { }
    }
    
    private static bool GetSymbolType(ISymbol symbol, out ITypeSymbol? result)
    {
        result = symbol switch
        {
            IFieldSymbol field => field.Type,
            IPropertySymbol property => property.Type,
            ILocalSymbol local => local.Type,
            IParameterSymbol parameter => parameter.Type,
            IMethodSymbol method => method.ReturnType,
            IEventSymbol eventSymbol => eventSymbol.Type,
            ITypeSymbol type => type,
            _ => null
        };

        return result is not null;
    }
    
    private static bool IsStruct(ISymbol symbol)
    {
        if (!GetSymbolType(symbol, out ITypeSymbol? result)) return false;
        if (result!.SpecialType != SpecialType.None) return false;
        return result.TypeKind == TypeKind.Struct;
    }

    private static bool GetParser(ISymbol symbol, in string input, in string output, out string value)
    {
        value = "Unsupported";
        if (!GetSymbolType(symbol, out ITypeSymbol? result)) return false;
        value = result!.SpecialType switch
        {
            SpecialType.System_Boolean => $"ParseBool({input}, ref {output});",
            SpecialType.System_Char    => $"{output} = char.Parse({input});",
            SpecialType.System_SByte   => $"{output} = sbyte.Parse({input});",
            SpecialType.System_Byte    => $"{output} = byte.Parse({input});",
            SpecialType.System_Int16   => $"{output} = short.Parse({input});",
            SpecialType.System_UInt16  => $"{output} = ushort.Parse({input});",
            SpecialType.System_Int32   => $"{output} = int.Parse({input});",
            SpecialType.System_UInt32  => $"{output} = uint.Parse({input});",
            SpecialType.System_Int64   => $"{output} = long.Parse({input});",
            SpecialType.System_UInt64  => $"{output} = ulong.Parse({input});",
            SpecialType.System_Single  => $"ParseFloat({input}, ref {output});",
            SpecialType.System_Double  => $"{output} = double.Parse({input});",
            SpecialType.System_Decimal => $"{output} = decimal.Parse({input});",
            SpecialType.None           => $"Enum.TryParse({input}, out {output});",
            _ => $"Unsupported special type: {result.SpecialType}"
        };
        return true;
        return !value.StartsWith('U');
    }
    
    public void Initialize(IncrementalGeneratorInitializationContext context)
    {
        IncrementalValuesProvider<StructDeclarationSyntax> structDeclarations = context.SyntaxProvider
            .CreateSyntaxProvider(
                predicate: static (s, _) => s is StructDeclarationSyntax { AttributeLists.Count: > 0 },
                transform: static (ctx, _) => (StructDeclarationSyntax)ctx.Node)
            .Where(static s => s != null);
        IncrementalValueProvider<(Compilation Left, ImmutableArray<StructDeclarationSyntax> Right)> compilationAndStructs = context.CompilationProvider.Combine(structDeclarations.Collect());
        context.RegisterSourceOutput(compilationAndStructs, 
            static (spc, source) => Execute(source.Left, source.Right, spc));
    }

    private static void Execute(Compilation compilation,
        IReadOnlyCollection<StructDeclarationSyntax> structs,
        SourceProductionContext context)
    {
        if (structs.Count == 0) return;
        
        List<INamedTypeSymbol> validStructs = new(20);
        foreach (StructDeclarationSyntax structDecl in structs)
        {
            SemanticModel model = compilation.GetSemanticModel(structDecl.SyntaxTree);
            if (model.GetDeclaredSymbol(structDecl) is not INamedTypeSymbol symbol) continue;
            bool hasAttribute = symbol.GetAttributes().Any(attr =>
                attr.AttributeClass?.Name == "IniSerializable");

            if (!hasAttribute) continue;

            if (symbol.DeclaredAccessibility != Accessibility.Public)
            {
                context.ReportDiagnostic(Diagnostic.Create(
                    new DiagnosticDescriptor(
                        "INI001",
                        "Invalid accessibility",
                        $"Struct '{symbol.ToDisplayString()}' must be public to use [IniSerializable]",
                        "Design",
                        DiagnosticSeverity.Error,
                        true),
                    structDecl.Identifier.GetLocation()));
                continue;
            }

            validStructs.Add(symbol);
        }

        if (validStructs.Count <= 0) return;
        string source = GenerateSource(ref validStructs);
        context.AddSource("TinyIni.generated.cs", SourceText.From(source, Encoding.UTF8));
    }

    private static string GenerateSource(ref List<INamedTypeSymbol> structs)
    {
        StringBuilder sb = new(4096);

        sb.AppendLine("using System;");
        sb.AppendLine("using System.Diagnostics;");
        sb.AppendLine("using System.Text;");
        sb.AppendLine("using System.Runtime.CompilerServices;");
        sb.AppendLine("");
        
        sb.AppendLine("public static partial class TinyIni");
        sb.AppendLine(ih.Open);
        sb.AppendLine($"{ih.Pad}[AttributeUsage(AttributeTargets.Struct)]");
        sb.AppendLine($"{ih.Pad}public class IniSerializable : Attribute {{ }}");
        GenerateSavePerType(ref sb, ref structs);
        GenerateLoadPerType(ref sb, ref structs);
        AppendConstantMethods(ref sb);
        sb.AppendLine(ih.Close);

        return sb.ToString();
    }
    
    /* SAVING
    ------------------------------------------------------------------------------- */

    private static void GenerateSavePerType(ref StringBuilder sb, ref List<INamedTypeSymbol> structs)
    {
        int i = 0;
        foreach (INamedTypeSymbol structSymbol in structs)
        {
            sb.AppendLine("");
            sb.AppendLine($"{ih.Pad}public static void Save(in string path, in {structSymbol.ToDisplayString()} data)");
            sb.AppendLine(ih.Open);
            sb.AppendLine($"{ih.Pad}StringBuilder sb = new(1024);");
            sb.AppendLine("");
            ImmutableArray<ISymbol> symbols = structSymbol.GetMembers();
            foreach (ISymbol member in symbols)
            {
                if (member.Kind != SymbolKind.Field) continue;
                if (IsStruct(member)) continue;
                sb.AppendLine($"{ih.Pad}sb.AppendLine($\"{member.Name}={{data.{member.Name}}}\");");
            }
            
            bool first = true;
            foreach (ISymbol member in symbols)
            {
                if (member.Kind != SymbolKind.Field) continue;
                if (!IsStruct(member)) continue;
                ITypeSymbol? memberType = member switch
                {
                    IFieldSymbol field => field.Type,
                    IPropertySymbol property => property.Type,
                    _ => null
                };
                if (memberType == null) continue;
                if (!memberType.IsValueType) continue;
                if (memberType is not INamedTypeSymbol { TypeKind: TypeKind.Struct } nestedStruct) continue;
                if (!first)
                {
                    sb.AppendLine("");
                    sb.AppendLine($"{ih.Pad}sb.AppendLine(\"\");");
                }
                first = false;
                sb.AppendLine($"{ih.Pad}sb.AppendLine(\"[{member.Name}]\");");
                foreach (ISymbol nMember in nestedStruct.GetMembers())
                {
                    if (nMember.Kind != SymbolKind.Field) continue;
                    sb.AppendLine($"{ih.Pad}sb.AppendLine($\"{nMember.Name}={{data.{member.Name}.{nMember.Name}}}\");");
                }
            }
            sb.AppendLine("");
            sb.AppendLine($"{ih.Pad}OverwriteData(sb, path);");
            sb.AppendLine(ih.Close);
            i++;
        }
    }
    
    /* LOADING
    ------------------------------------------------------------------------------- */
    
    private static void GenerateLoadPerType(ref StringBuilder sb, ref List<INamedTypeSymbol> structs)
    {
        int i = 0;
        foreach (INamedTypeSymbol structSymbol in structs)
        {
            ImmutableArray<ISymbol> symbols = structSymbol.GetMembers();
            sb.AppendLine("");
            sb.AppendLine($"{ih.Pad}/// <returns>true when loaded an existing file successfully, false when the file did not exist and a new one was created instead.</returns>");
            sb.AppendLine($"{ih.Pad}public static bool Load(in string path, ref {structSymbol.ToDisplayString()} data)");
            sb.AppendLine(ih.Open);
            sb.AppendLine($"{ih.Pad}if (!Directory.Exists(Path.GetDirectoryName(path)) || !File.Exists(path))");
            sb.AppendLine(ih.Open);
            sb.AppendLine($"{ih.Pad}#if DEBUG");
            sb.AppendLine($"{ih.Pad}Console.WriteLine($\"[TinyIni] File \\\"{{path}}\\\" does not exist, creating a new one\");");
            sb.AppendLine($"{ih.Pad}#endif");
            sb.AppendLine($"{ih.Pad}Save(in path, in data);");
            sb.AppendLine($"{ih.Pad}return false;");
            sb.AppendLine(ih.Close);
            sb.AppendLine($"{ih.Pad}using FileStream sourceStream = File.Open(path, FileMode.Open);");
            sb.AppendLine($"{ih.Pad}using StreamReader reader = new(sourceStream);");
            sb.AppendLine($"{ih.Pad}ReadOnlySpan<char> currentSection = null;");
            sb.AppendLine($"{ih.Pad}ReadOnlySpan<char> currentKey = null;");
            sb.AppendLine($"{ih.Pad}ReadOnlySpan<char> currentValue = null;");
            sb.AppendLine($"{ih.Pad}while (reader.ReadLine() is {{ }} sLine)");
            sb.AppendLine(ih.Open);
            sb.AppendLine($"{ih.Pad}if (sLine.Length == 0) continue;");
            sb.AppendLine($"{ih.Pad}ReadOnlySpan<char> line = sLine.AsSpan();");
            sb.AppendLine($"{ih.Pad}switch (line[0])");
            sb.AppendLine(ih.Open);
            
            sb.AppendLine($"{ih.Pad}case ';': case '#':");
            ih.Depth += 1;
            sb.AppendLine($"{ih.Pad}continue;");
            ih.Depth -= 1;
            
            sb.AppendLine($"{ih.Pad}case '[':");
            ih.Depth += 1;
            sb.AppendLine($"{ih.Pad}currentSection = line[1..line.LastIndexOf(']')];");
            sb.AppendLine($"{ih.Pad}continue;");
            ih.Depth -= 1;
            
            sb.AppendLine($"{ih.Pad}default:");
            ih.Depth += 1;
            sb.AppendLine($"{ih.Pad}GetKeyValue(in line, ref currentKey, ref currentValue);");
            sb.AppendLine($"{ih.Pad}AssignKeyValue(ref data, in currentSection, in currentKey, in currentValue);");
            sb.AppendLine($"{ih.Pad}continue;");
            ih.Depth -= 1;
            
            sb.AppendLine(ih.Close);
            sb.AppendLine(ih.Close);
            sb.AppendLine($"{ih.Pad}return true;");
            sb.AppendLine(ih.Close);
            sb.AppendLine("");
            sb.AppendLine($"{ih.Pad}private static void AssignKeyValue(ref {structSymbol.ToDisplayString()} outData, in ReadOnlySpan<char> section, in ReadOnlySpan<char> key, in ReadOnlySpan<char> value)");
            sb.AppendLine(ih.Open);
            sb.AppendLine($"{ih.Pad}switch (section)");
            sb.AppendLine(ih.Open);
            sb.AppendLine($"{ih.Pad}case \"\":");
            ih.Depth += 1;
            
            sb.AppendLine($"{ih.Pad}switch (key)");
            sb.AppendLine(ih.Open);
            foreach (ISymbol member in symbols)
            {
                if (member.Kind != SymbolKind.Field) continue;
                if (IsStruct(member)) continue;
                if (!GetParser(member, "value", $"outData.{member.Name}", out string parser)) continue;
                sb.AppendLine($"{ih.Pad}case \"{member.Name}\":");
                ih.Depth += 1;
                sb.AppendLine($"{ih.Pad}{parser}");
                sb.AppendLine($"{ih.Pad}break;");
                ih.Depth -= 1;
            }
            sb.AppendLine(ih.Close);
            sb.AppendLine($"{ih.Pad}break;");
            ih.Depth -= 1;

            foreach (ISymbol member in symbols)
            {
                if (member.Kind != SymbolKind.Field) continue;
                if (!IsStruct(member)) continue;
                ITypeSymbol? memberType = member switch
                {
                    IFieldSymbol field => field.Type,
                    IPropertySymbol property => property.Type,
                    _ => null
                };
                if (memberType == null) continue;
                if (!memberType.IsValueType) continue;
                if (memberType is not INamedTypeSymbol { TypeKind: TypeKind.Struct } nestedStruct) continue;
                
                sb.AppendLine($"{ih.Pad}case \"{member.Name}\":");
                
                ih.Depth += 1;
                sb.AppendLine($"{ih.Pad}switch (key)");
                sb.AppendLine(ih.Open);
                foreach (ISymbol nMember in nestedStruct.GetMembers())
                {
                    if (nMember.Kind != SymbolKind.Field) continue;
                    if (!GetParser(nMember, "value", $"outData.{member.Name}.{nMember.Name}", out string parser)) continue;
                    sb.AppendLine($"{ih.Pad}case \"{nMember.Name}\":");
                    ih.Depth += 1;
                    sb.AppendLine($"{ih.Pad}{parser}");
                    sb.AppendLine($"{ih.Pad}break;");
                    ih.Depth -= 1;
                }
                sb.AppendLine(ih.Close);
                sb.AppendLine($"{ih.Pad}break;");
                ih.Depth -= 1;
            }
            sb.AppendLine(ih.Close);
            sb.AppendLine(ih.Close);
            i++;
        }
    }
    
    /* SHARED
    ------------------------------------------------------------------------------- */
    
    private static void AppendConstantMethods(ref StringBuilder sb)
    {
        sb.Append("""
                  
                      private static async void OverwriteData(StringBuilder sb, string path)
                      {
                          try
                          {
                              PopulatePath(path);
                              byte[] encoded = Encoding.UTF8.GetBytes(sb.ToString());
                              await using FileStream sourceStream = File.Open(path, FileMode.OpenOrCreate);
                              sourceStream.SetLength(0);
                              await sourceStream.WriteAsync(encoded, 0, encoded.Length);
                          }
                          catch (Exception e)
                          {
                              Console.WriteLine($"[TinyIni] Failed to write data to file {path}\n{e}");
                          }
                      }
                      
                      private static void PopulatePath(string path)
                      {
                          string? directoryPath = Path.GetDirectoryName(path);
                          if (string.IsNullOrEmpty(directoryPath)) return;
                          if (!Directory.Exists(directoryPath)) Directory.CreateDirectory(directoryPath);
                      }
                      
                      // ReSharper disable twice RedundantAssignment
                      [MethodImpl(MethodImplOptions.AggressiveInlining)]
                      private static void GetKeyValue(in ReadOnlySpan<char> line, ref ReadOnlySpan<char> outKey, ref ReadOnlySpan<char> outValue)
                      {
                          int equalsPos = line.IndexOf('=');
                          outKey = line[..equalsPos].Trim();
                          outValue = line[(equalsPos + 1)..].Trim();
                      }
                  
                      private static bool ParseBool(in ReadOnlySpan<char> value, ref bool outValue)
                      {
                          const string TRUE_CHARACTERS = "1TtYy";
                          const string FALSE_CHARACTERS = "0FfNn";
                          
                          foreach (char character in TRUE_CHARACTERS)
                          {
                              if (value[0] != character) continue;
                              outValue = true;
                              return true;
                          }
                          
                          foreach (char character in FALSE_CHARACTERS)
                          {
                              if (value[0] != character) continue;
                              outValue = false;
                              return true;
                          }
                          
                          return false;
                      }
                      
                      private static bool ParseFloat(in ReadOnlySpan<char> value, ref float outValue)
                      {
                          if (float.TryParse(value, out outValue)) return true;
                      
                          Span<char> value2 = stackalloc char[value.Length];
                          int length = 0;
                          int pointIndex = 0;
                          
                          for (int i = value.Length - 1; i >= 0; i--)
                          {
                              if (value[i] is not ('.' or ',')) continue;
                              pointIndex = i;
                              break;
                          }
                      
                          for (int i = 0; i < value.Length; i++)
                          {
                              if (i == pointIndex)
                              {
                                  value2[length] = '.';
                                  length += 1;
                                  continue;
                              }
                              if (value[i] is '.' or ',' or >= 'A' and <= 'z') continue;
                              value2[length] = value[i];
                              length += 1;
                          }
                          
                          return float.TryParse(value2[..length], out outValue);
                      }
                  
                  """);
    }
}