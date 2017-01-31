unit database;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, Game;

type

  { TDatabase }

  TDatabase = class(specialize TFPGObjectList<TGame>)
  public
    procedure LoadFromFile(const AFile: string);
    procedure LoadFromStream(const AStream: TStream); virtual; abstract;
    procedure SaveToFile(const AFile: string);
    procedure SaveToStream(const AStream: TStream); virtual; abstract;
  end;

implementation

{ TDatabase }

procedure TDatabase.LoadFromFile(const AFile: string);
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  ms.LoadFromFile(AFile);
  Self.LoadFromStream(ms);
  ms.Free;
end;

procedure TDatabase.SaveToFile(const AFile: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AFile, fmCreate);
  Self.SaveToStream(fs);
  fs.Free;
end;

end.

