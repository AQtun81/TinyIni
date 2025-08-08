# Tiny Ini
Is a small source generator which generates methods for serializing and deserializing structs.


### Serialization
```cs
[TinyIni.IniSerializable]
public struct Config
{
  public int Version = 0;
  public Display Display = new()
  {
    Height = 1080,
    Width = 1920,
    VSync = VSync.Enabled
  };

  public Graphics Graphics = new()
  {
    MSAA = 16,
    Advanced = new AdvancedGraphics
    {
      GraphicsAPI = GraphicsAPI.Vulkan
    }
  };
  public Input InputKeyboard = new()
  {
    JumpKey = 32
  };
  public Input InputGamepad = new()
  {
    JumpKey = 16
  };

  public Config() {}
}
```

<details>
<summary>[omitted for brevity]</summary>

```cs
public struct Graphics
{
  public byte MSAA;
  public AdvancedGraphics Advanced;
}

public struct AdvancedGraphics
{
  public GraphicsAPI GraphicsAPI;
}

public struct Display
{
  public int Width;
  public int Height;
  public VSync VSync;
}

public struct Input
{
  public byte JumpKey;
}

public enum VSync
{
  Enabled,
  Disabled,
  Adaptive
}

public enum GraphicsAPI
{
  Vulkan,
  DirectX12
}
```

</details>

```cs
TinyIni.Save("config.ini", in config);
```
<sub>config.ini</sub>
```ini
Version=0

[Display]
Width=1920
Height=1080
VSync=Enabled

[Graphics]
MSAA=16

[Graphics/Advanced]
GraphicsAPI=Vulkan

[InputKeyboard]
JumpKey=32

[InputGamepad]
JumpKey=16
```

### Deserialization
```cs
TinyIni.Load("config.ini", ref config);
```


# Installation

### Clone

Clone this repository into your solution directory. </br>
`git clone https://github.com/AQtun81/TinyIni`

### Add analyzer reference in your project
```xml
<ItemGroup>
  <ProjectReference Include="..\TinyIni\TinyIni.csproj"
                    OutputItemType="Analyzer"
                    ReferenceOutputAssembly="false"/>
</ItemGroup>
```

### Compile your project

You won't see IntelliSense for any generated code until the project is built at least once.

### Add `[TinyIni.IniSerializable]` attribute above any struct you wish to serialize

```cs
[TinyIni.IniSerializable]
public struct Config
{
  // ...
```

### Use the generated Load and Save methods
```cs
// this will additionally create a new file if one isn't already present
TinyIni.Load($"config.ini", ref config);

// this will generate new fields and remove obsolete ones
TinyIni.Save($"config.ini", in config);
```

# FAQ
### Unity support
You'll have to do some additional steps in order to use Tiny Ini with Unity.

1. `git clone https://github.com/AQtun81/TinyIni`
2. Inside of `TinyIni.csproj` downgrade `Microsoft.CodeAnalysis.Common` and `Microsoft.CodeAnalysis.CSharp` to `4.3.0-3.final`
3. Uncomment `//#define TINY_INI_COMPATIBILITY` inside of `Program.cs` to enable support for older versions of C#
4. Build the project for release `dotnet build -c release`
5. Copy the dll file to somewhere inside of your `Assets` folder
6. Click on the .dll file to open the Plugin Inspector window
7. Uncheck `Any Platform`, `Editor` and `Standalone` checkmarks, under Asset Labels create and assign a new label called `RoslynAnalyzer`

Refer to [Unity's documentation](https://docs.unity3d.com/Manual/create-source-generator.html) for more details.

### Godot support
Godot 4 C# projects use MSBuild so following the default instructions for C# projects will work. </br>
Versions prior to that are not supported.
