unit NotationMemo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, RichMemo, Game;

type

  { TNotationMemo }

  TNotationMemo = class(TRichMemo)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    procedure SetTextFromGameNotation(const AGameNotation: TGameNotation);
  published
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I notationmemo_icon.lrs}
  RegisterComponents('Chess',[TNotationMemo]);
end;

{ TNotationMemo }

procedure TNotationMemo.SetTextFromGameNotation(
  const AGameNotation: TGameNotation);
begin

end;

end.
