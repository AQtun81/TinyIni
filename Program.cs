//#define TINY_INI_DEBUG

using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
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
        public ushort Depth;

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
    }
    
    private static bool GetSymbolType(in ISymbol symbol, out ITypeSymbol? result)
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
    
    private struct SymbolInfo
    {
        public SpecialType SpecialType;
        public bool IsStruct;
        public bool IsEnum;
        public bool IsValueType;
        public ITypeSymbol? TypeSymbol;
    }

    private static SymbolInfo ProbeInfo(in ISymbol symbol)
    {
        SymbolInfo result = new();
        if (!GetSymbolType(in symbol, out ITypeSymbol? typeSymbol))
        {
            result.SpecialType = SpecialType.None;
            result.IsStruct = false;
            result.IsEnum = false;
            result.IsValueType = false;
            result.TypeSymbol = null;
            return result;
        }
        result.SpecialType = typeSymbol!.SpecialType;
        result.IsStruct = typeSymbol.SpecialType == SpecialType.None && typeSymbol.TypeKind == TypeKind.Struct;
        result.IsEnum = !result.IsStruct && typeSymbol.BaseType?.SpecialType == SpecialType.System_Enum;
        result.IsValueType = typeSymbol.IsValueType;
        result.TypeSymbol = typeSymbol;
        return result;
    }
    
    [Conditional("TINY_INI_DEBUG")]
    private static void AppendInfo(ref StringBuilder sb, SymbolInfo info)
    {
        #if TINY_INI_DEBUG
        sb.AppendLine($"{ih.Pad}// IsStruct: {info.IsStruct}");
        sb.AppendLine($"{ih.Pad}// SpecialType: {info.SpecialType}");
        sb.AppendLine($"{ih.Pad}// IsValueType: {info.IsValueType}");
        sb.AppendLine($"{ih.Pad}// TypeSymbol: {info.TypeSymbol}");
        sb.AppendLine($"{ih.Pad}// TypeKind: {info.TypeSymbol?.TypeKind}");
        sb.AppendLine($"{ih.Pad}// BaseType: {info.TypeSymbol?.BaseType}");
        sb.AppendLine($"{ih.Pad}// IsEnum: {info.IsEnum}");
        #endif
    }

    private static bool GetParser(ref SymbolInfo symbol, in string input, in string output, out string value)
    {
        value = symbol.SpecialType switch
        {
            SpecialType.System_Boolean => $"ParseBool({input}, ref {output});",
            SpecialType.System_Char    => $"ParseChar({input}, ref {output});",
            SpecialType.System_SByte   => $"ParseInteger({input}, ref {output});",
            SpecialType.System_Byte    => $"ParseUnsignedInteger({input}, ref {output});",
            SpecialType.System_Int16   => $"ParseInteger({input}, ref {output});",
            SpecialType.System_UInt16  => $"ParseUnsignedInteger({input}, ref {output});",
            SpecialType.System_Int32   => $"ParseInteger({input}, ref {output});",
            SpecialType.System_UInt32  => $"ParseUnsignedInteger({input}, ref {output});",
            SpecialType.System_Int64   => $"ParseInteger({input}, ref {output});",
            SpecialType.System_UInt64  => $"ParseUnsignedInteger({input}, ref {output});",
            SpecialType.System_Single  => $"ParseFloat({input}, ref {output});",
            SpecialType.System_Double  => $"ParseFloat({input}, ref {output});",
            SpecialType.System_Decimal => $"ParseFloat({input}, ref {output});",
            SpecialType.System_String  => $"ParseString({input}, ref {output});",
            SpecialType.None           => $"Enum.TryParse({input}, out {output});",
            _ => ""
        };
        return value.Length != 0;
    }
    
    public void Initialize(IncrementalGeneratorInitializationContext context)
    {
        IncrementalValuesProvider<StructDeclarationSyntax> structDeclarations = context.SyntaxProvider
            .CreateSyntaxProvider(
                predicate: static (s, _) => s is StructDeclarationSyntax { AttributeLists: { Count: > 0 } },
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
            bool hasAttribute = false;
            foreach (AttributeData? attr in symbol.GetAttributes())
            {
                if (attr.AttributeClass?.Name != "IniSerializable") continue;
                hasAttribute = true;
                break;
            }

            if (!hasAttribute) continue;

            if (symbol.DeclaredAccessibility != Accessibility.Public)
            {
                context.ReportDiagnostic(Diagnostic.Create(
                    new DiagnosticDescriptor(
                        "[TinyIni]",
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
        StringBuilder sb = new(32768);
        StringBuilder tempBuilder = new(1024);
        StringBuilder sectionBuilder = new(1024);

        sb.AppendLine("using System;");
        sb.AppendLine("using System.Diagnostics;");
        sb.AppendLine("using System.Text;");
        sb.AppendLine("using System.Runtime.CompilerServices;");
        sb.AppendLine("");
        
        sb.AppendLine("public static partial class TinyIni");
        sb.AppendLine(ih.Open);
        sb.AppendLine($"{ih.Pad}[AttributeUsage(AttributeTargets.Struct)]");
        sb.AppendLine($"{ih.Pad}public class IniSerializable : Attribute {{ }}");
        GenerateSavePerType(ref sb, ref tempBuilder, ref sectionBuilder, ref structs);
        GenerateLoadPerType(ref sb, ref tempBuilder, ref sectionBuilder, ref structs);
        AppendConstantMethods(ref sb);
        sb.AppendLine(ih.Close);

        return sb.ToString();
    }
    
    /* SAVING
    ------------------------------------------------------------------------------- */
    
    private static void GenerateSavePerType(ref StringBuilder sb, ref StringBuilder fieldBuilder, ref StringBuilder sectionBuilder, ref List<INamedTypeSymbol> structs)
    {
        foreach (INamedTypeSymbol structSymbol in structs)
        {
            int predictedSize = 0;
            sb.AppendLine("");
            sb.AppendLine($"{ih.Pad}public static void Save(in string path, in {structSymbol.ToDisplayString()} data)");
            sb.AppendLine(ih.Open);
            sb.Append($"{ih.Pad}StringBuilder sb = new(");
            int predictedSizePos = sb.Length;
            sb.AppendLine(" + 64);");
            sb.AppendLine("");
            GenerateSavePerTypeInnerFunction(ref sb, ref fieldBuilder, ref sectionBuilder, in structSymbol, ref predictedSize);
            sb.AppendLine("");
            sb.Insert(predictedSizePos, predictedSize);
            sb.AppendLine($"{ih.Pad}OverwriteData(sb, path);");
            sb.AppendLine(ih.Close);
        }
    }

    private struct SymbolInfoPair
    {
        public ISymbol Symbol;
        public SymbolInfo Info;
    }

    private static void GenerateSavePerTypeInnerFunction(ref StringBuilder sb, ref StringBuilder fieldBuilder, ref StringBuilder sectionBuilder, in INamedTypeSymbol structSymbol, ref int predictedSize)
    {
        List<SymbolInfoPair> structSymbols = new(10);
        ImmutableArray<ISymbol> symbols = structSymbol.GetMembers();
        foreach (ISymbol member in symbols)
        {
            if (member.Kind != SymbolKind.Field) continue;
            SymbolInfo memberInfo = ProbeInfo(member);
            if (memberInfo.IsStruct)
            {
                structSymbols.Add(new SymbolInfoPair {Info = memberInfo, Symbol = member});
                continue;
            }
            if (memberInfo.SpecialType == SpecialType.System_String)
            {
                AppendInfo(ref sb, memberInfo);
                sb.AppendLine($"{ih.Pad}sb.AppendLine($\"{member.Name}=\\\"{{data.{fieldBuilder}{member.Name}}}\\\"\");");
                predictedSize += fieldBuilder.Length + member.Name.Length + 32;
            }
            else
            {
                AppendInfo(ref sb, memberInfo);
                sb.AppendLine($"{ih.Pad}sb.AppendLine($\"{member.Name}={{data.{fieldBuilder}{member.Name}}}\");");
                predictedSize += fieldBuilder.Length + member.Name.Length + 8;
            }
        }

        foreach (SymbolInfoPair member in structSymbols)
        {
            if (member.Info.TypeSymbol is not INamedTypeSymbol { TypeKind: TypeKind.Struct } nestedStruct) continue;
            int pos = fieldBuilder.Length;
            int sPos = sectionBuilder.Length;
            fieldBuilder.Append($"{member.Symbol.Name}.");
            if (sPos > 0) sectionBuilder.Append('/');
            sectionBuilder.Append($"{member.Symbol.Name}");
            sb.AppendLine("");
            sb.AppendLine($"{ih.Pad}sb.AppendLine(\"\");");
            sb.AppendLine($"{ih.Pad}sb.AppendLine(\"[{sectionBuilder}]\");");
            GenerateSavePerTypeInnerFunction(ref sb, ref fieldBuilder, ref sectionBuilder, in nestedStruct, ref predictedSize);
            fieldBuilder.Length = pos;
            sectionBuilder.Length = sPos;
        }
    }
    
    /* LOADING
    ------------------------------------------------------------------------------- */
    
    private static void GenerateLoadPerType(ref StringBuilder sb, ref StringBuilder fieldBuilder, ref StringBuilder sectionBuilder, ref List<INamedTypeSymbol> structs)
    {
        foreach (INamedTypeSymbol structSymbol in structs)
        {
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

            GenerateLoadPerTypeInnerFunction(ref sb, ref fieldBuilder, ref sectionBuilder, in structSymbol);
            
            ih.Depth -= 1;
            sb.AppendLine(ih.Close);
            sb.AppendLine(ih.Close);
        }
    }
    
    private static void GenerateLoadPerTypeInnerFunction(ref StringBuilder sb, ref StringBuilder fieldBuilder, ref StringBuilder sectionBuilder, in INamedTypeSymbol structSymbol)
    {
        List<SymbolInfoPair> structSymbols = new(10);
        ImmutableArray<ISymbol> symbols = structSymbol.GetMembers();
        
        sb.AppendLine($"{ih.Pad}switch (key)");
        sb.AppendLine(ih.Open);
        foreach (ISymbol member in symbols)
        {
            if (member.Kind != SymbolKind.Field) continue;
            SymbolInfo memberInfo = ProbeInfo(member);
            if (memberInfo.IsStruct)
            {
                structSymbols.Add(new SymbolInfoPair {Info = memberInfo, Symbol = member});
                continue;
            }
            if (!GetParser(ref memberInfo, "value", $"outData.{fieldBuilder}{member.Name}", out string parser)) continue;
            AppendInfo(ref sb, memberInfo);
            sb.AppendLine($"{ih.Pad}case \"{member.Name}\":");
            ih.Depth += 1;
            sb.AppendLine($"{ih.Pad}{parser}");
            sb.AppendLine($"{ih.Pad}break;");
            ih.Depth -= 1;
        }
        sb.AppendLine(ih.Close);
        sb.AppendLine($"{ih.Pad}break;");

        foreach (SymbolInfoPair member in structSymbols)
        {
            if (member.Info.TypeSymbol is not INamedTypeSymbol { TypeKind: TypeKind.Struct } nestedStruct) continue;
            int pos = fieldBuilder.Length;
            int sPos = sectionBuilder.Length;
            fieldBuilder.Append($"{member.Symbol.Name}.");
            if (sPos > 0) sectionBuilder.Append('/');
            sectionBuilder.Append($"{member.Symbol.Name}");
            ih.Depth -= 1;
            sb.AppendLine($"{ih.Pad}case \"{sectionBuilder}\":");
            ih.Depth += 1;
            
            GenerateLoadPerTypeInnerFunction(ref sb, ref fieldBuilder, ref sectionBuilder, in nestedStruct);
                
            fieldBuilder.Length = pos;
            sectionBuilder.Length = sPos;
        }
    }
    
    /* SHARED
    ------------------------------------------------------------------------------- */
    
    private static void AppendConstantMethods(ref StringBuilder sb)
    {
        sb.Append(@"
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
            Console.WriteLine($""[TinyIni] Failed to write data to file {path}\n{e}"");
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
        if (value.Length <= 0) return false;
        const string TRUE_CHARACTERS = ""1TtYy"";
        const string FALSE_CHARACTERS = ""0FfNn"";
        
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

    private static bool ParseString(in ReadOnlySpan<char> value, ref string outValue)
    {
        if (value.Length <= 0) return false;
        int first = value.IndexOf('""');
        if (first == -1) return false;
        first += 1;
        int next = value[first..].LastIndexOf('""');
        if (next == -1) return false;
        next += first;
        outValue = value[first..next].ToString();
        return true;
    }

    private static bool ParseChar(in ReadOnlySpan<char> value, ref char outValue)
    {
        if (value.Length <= 0) return false;
        outValue = value[0];
        return true;
    }

    private static bool ParseInteger(in ReadOnlySpan<char> value, ref long outValue)
    {
        if (long.TryParse(value, out outValue)) return true;
        if (value.Length <= 0) return false;
        
        Span<char> stripped = stackalloc char[value.Length];
        int strippedPos = 0;
        int startIndex = 0;
        if (value[0] == '-')
        {
            startIndex = 1;
            stripped[0] = '-';
            strippedPos = 1;
        }

        for (int i = startIndex; i < value.Length; i++)
        {
            ref readonly char character = ref value[i];
            if (character is < '0' or > '9') continue;
            stripped[strippedPos] = character;
            strippedPos += 1;
        }
        
        return long.TryParse(stripped, out outValue);
    }

    private static bool ParseUnsignedInteger(in ReadOnlySpan<char> value, ref ulong outValue)
    {
        if (ulong.TryParse(value, out outValue)) return true;
        if (value.Length <= 0) return false;
        
        Span<char> stripped = stackalloc char[value.Length];
        int strippedPos = 0;

        foreach (char character in value)
        {
            if (character is < '0' or > '9') continue;
            stripped[strippedPos] = character;
            strippedPos += 1;
        }
        
        return ulong.TryParse(stripped, out outValue);
    }
");
        AppendFloatMethod(ref sb, "float");
        AppendFloatMethod(ref sb, "double");
        AppendFloatMethod(ref sb, "decimal");
        AppendIntegerMethod(ref sb, "int");
        AppendIntegerMethod(ref sb, "short");
        AppendIntegerMethod(ref sb, "sbyte");
        AppendUnsignedIntegerMethod(ref sb, "uint");
        AppendUnsignedIntegerMethod(ref sb, "ushort");
        AppendUnsignedIntegerMethod(ref sb, "byte");
    }

    private static void AppendFloatMethod(ref StringBuilder sb, string type)
    {
        sb.Append($@"
    private static bool ParseFloat(in ReadOnlySpan<char> value, ref {type} outValue)
    {{
        if (value.Length <= 0) return false;
        if ({type}.TryParse(value, out outValue)) return true;
    
        Span<char> value2 = stackalloc char[value.Length];
        int length = 0;
        int pointIndex = 0;
        
        for (int i = value.Length - 1; i >= 0; i--)
        {{
            if (value[i] is not ('.' or ',')) continue;
            pointIndex = i;
            break;
        }}
    
        for (int i = 0; i < value.Length; i++)
        {{
            if (i == pointIndex)
            {{
                value2[length] = '.';
                length += 1;
                continue;
            }}
            if (value[i] is '.' or ',' or >= 'A' and <= 'z') continue;
            value2[length] = value[i];
            length += 1;
        }}
        
        return {type}.TryParse(value2[..length], out outValue);
    }}
");
    }

    private static void AppendIntegerMethod(ref StringBuilder sb, string type)
    {
        sb.Append($@"
    public static bool ParseInteger(in ReadOnlySpan<char> value, ref {type} outValue)
    {{
        long temp = outValue;
        bool success = ParseInteger(in value, ref temp);
        if (!success) return false;
        outValue = ({type})long.Clamp(temp, {type}.MinValue, {type}.MaxValue);
        return true;
    }}
");
    }

    private static void AppendUnsignedIntegerMethod(ref StringBuilder sb, string type)
    {
        sb.Append($@"
    public static bool ParseUnsignedInteger(in ReadOnlySpan<char> value, ref {type} outValue)
    {{
        ulong temp = outValue;
        bool success = ParseUnsignedInteger(in value, ref temp);
        if (!success) return false;
        outValue = ({type})ulong.Clamp(temp, {type}.MinValue, {type}.MaxValue);
        return true;
    }}
");
    }
}