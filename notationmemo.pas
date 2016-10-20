unit NotationMemo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  RichMemo, Game, Ply, StdCtrls, RichMemoUtils, fgl;

type

  TNotationStyle = record
    Color: TColor;
    Style: TFontStyles;
  end;

  TLineStyle = record
    CommentaryStyle: TNotationStyle;
    MoveStyle: TNotationStyle;
    NAGStyle: TNotationStyle;
    NumberStyle: TNotationStyle;
  end;

  PLineStyle = ^TLineStyle;

  TLineStyleList = specialize TFPGList<PLineStyle>;

  { TNotationMemo }

  TNotationMemo = class(TCustomRichMemo)
  private
    FLineStyles: TLineStyleList;
  protected
    { Protected declarations }
  public
    function AddLineStyle: PLineStyle;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InsertNotation(const TextUTF8: string; ANotationStyle: TNotationStyle;
      InsPos: integer = -1);
    procedure SetTextFromGame(const AGame: TGame);
  public
    property LineStyles: TLineStyleList read FLineStyles;
  published
    property Align;
    property Alignment;
    property Anchors;
    property BidiMode;
    property BorderSpacing;
    property BorderStyle;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property Lines;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnLinkAction;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnSelectionChange;
    property OnStartDrag;
    property OnPrintAction;
    property OnUTF8KeyPress;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property PopupMenu;
    property ParentShowHint;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property ZoomFactor;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I notationmemo_icon.lrs}
  RegisterComponents('Chess', [TNotationMemo]);
end;

{ TNotationMemo }

function TNotationMemo.AddLineStyle: PLineStyle;
var
  LineStyle: PLineStyle;
begin
  New(LineStyle);
  FLineStyles.Add(LineStyle);
  Result := LineStyle;
end;

constructor TNotationMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.ReadOnly := True;
  Self.ScrollBars := ssAutoVertical;
  Self.WordWrap := True;

  FLineStyles := TLineStyleList.Create;
end;

destructor TNotationMemo.Destroy;
var
  Style: PLineStyle;
begin
  for Style in FLineStyles do
    Dispose(Style);
  FLineStyles.Free;
  inherited Destroy;
end;

procedure TNotationMemo.InsertNotation(const TextUTF8: string;
  ANotationStyle: TNotationStyle; InsPos: integer);
begin
  InsertColorStyledText(Self, TextUTF8, ANotationStyle.Color,
    ANotationStyle.Style, InsPos);
end;

procedure TNotationMemo.SetTextFromGame(const AGame: TGame);
var
  Token: PToken;
  VarLevel: integer;
  Style: TLineStyle;
  AGameNotation: TGameNotation;
begin
  AGameNotation := AGame.GetGameNotation;
  VarLevel := -1;
  if FLineStyles.Count > 0 then
    Style := FLineStyles.items[0]^
  else
  begin
    Style.CommentaryStyle.Color := clBlack;
    Style.CommentaryStyle.Style := [];
    Style.MoveStyle.Color := clBlack;
    Style.MoveStyle.Style := [];
    Style.NAGStyle.Color := clBlack;
    Style.NAGStyle.Style := [];
    Style.NumberStyle.Color := clBlack;
    Style.NumberStyle.Style := [];
  end;
  Self.Lines.Clear;
  for Token in AGameNotation do
  begin
    case Token^.Kind of
      tkMove: InsertNotation(Token^.Value + ' ', Style.MoveStyle);
      tkNumber: InsertNotation(Token^.Value, Style.NumberStyle);
      tkBeginLine:
      begin
        Inc(VarLevel);
        if VarLevel < FLineStyles.Count then
          Style := FLineStyles.Items[VarLevel]^;
        if VarLevel >= 1 then
          InsertNotation(LineEnding + '( ', Style.MoveStyle);
      end;
      tkEndLine:
      begin
        if VarLevel > 0 then
        begin
          InsertNotation(') ' + LineEnding, Style.MoveStyle);
          Style := FLineStyles.Items[VarLevel - 1]^;
        end;
        Dec(VarLevel);
      end;
      tkComment: InsertNotation(Token^.Value + ' ', Style.CommentaryStyle);
      tkNAG: InsertNotation(NAGToStr(StrToInt(Token^.Value)) +
          ' ', Style.NAGStyle);
      tkResult: InsertNotation(Token^.Value, Style.MoveStyle);
    end;
  end;
  AGameNotation.ClearAndFree;
end;

end.
