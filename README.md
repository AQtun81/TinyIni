# Tiny Ini
Is a small source generator which generates methods for serializing and deserializing structs. <br/>
It is very simple with only 2 functions and a single attribute.
### Add analyzer reference in your project
```xml
<ItemGroup>
  <ProjectReference Include="..\TinyIni\TinyIni.csproj"
                    OutputItemType="Analyzer"
                    ReferenceOutputAssembly="false"/>
</ItemGroup>
```

### Add `TinyIni.IniSerializable` attribute above any struct you wish to serialize
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
```
### Use the generated Load and Save methods
```cs
string exePath = $"{Path.GetDirectoryName(AppContext.BaseDirectory)}{Path.DirectorySeparatorChar}";
// this will additionally create a new config if one isn't present
TinyIni.Load($"{exePath}config.ini", ref config);
if (config.Version != CURRENT_CONFIG_VERSION)
{
  config.Version = CURRENT_CONFIG_VERSION;
  // this will generate new fields and remove obsolete ones
  TinyIni.Save($"{exePath}config.ini", in config);
}
```
### This serialized struct from this example will output the following ini file
```ini
Version=0

[Display]
Width=1920
Height=1080
VSync=Enabled

[InputKeyboard]
JumpKey=32

[InputGamepad]
JumpKey=16
```