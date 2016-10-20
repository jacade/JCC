{ JCC (Jan's Chess Componenents) - This file contains a memo to display game notation
  Copyright (C) 2016  Jan Dette

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
}

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
    // If True, the commentary will be in a new line using the indent
    CommentaryNewLine: boolean;
    CommentaryIndent: integer;
    // defines if the line should start in a new text line
    NeedsNewLine: boolean;
    // gives an indent for this line
    LineIndent: integer;
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
    procedure ClearLineStyles;
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

procedure TNotationMemo.ClearLineStyles;
var
  Style: PLineStyle;
begin
  for Style in FLineStyles do
    Dispose(Style);
  FLineStyles.Clear;
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
  VarLevel, Start: integer;
  Style: TLineStyle;
  AGameNotation: TGameNotation;
  AtStartOfLine: boolean; // Is true, when last char was a line ending
  m: TParaMetric;
  // sums up the indents of the line styles
  CommentaryIndent: integer;
  // sums up the indents of the line styles
  LineIndent: integer;

  procedure BeginParagraph;
  begin
    Start := Length(Self.Text);
  end;

  procedure EndParagraph(const Indent: integer);
  begin
    m.HeadIndent := Indent;
    m.FirstLine := m.HeadIndent;
    Self.SetRangeParaParams(Start + 1, Length(Self.Text) - Start,
      [pmm_HeadIndent, pmm_FirstLine], m);
    WriteLn(Start, ' ', Length(Self.Text));
  end;

begin
  AtStartOfLine := False;
  AGameNotation := AGame.GetGameNotation;
  Start := 0;
  CommentaryIndent := 0;
  LineIndent := 0;
  InitParaMetric(m);
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
      tkMove:
      begin
        InsertNotation(Token^.Value + ' ', Style.MoveStyle);
        AtStartOfLine := False;
      end;
      tkNumber:
      begin
        InsertNotation(Token^.Value, Style.NumberStyle);
        AtStartOfLine := False;
      end;
      tkBeginLine:
      begin
        Inc(VarLevel);
        if VarLevel < FLineStyles.Count then
          Style := FLineStyles.Items[VarLevel]^;
        if Style.NeedsNewLine then
        begin
          EndParagraph(LineIndent);
          if not AtStartOfLine then
          begin
            InsertNotation(LineEnding, Style.MoveStyle);
          end
          else
            AtStartOfLine := False;
        end;
        BeginParagraph;
        InsertNotation('( ', Style.MoveStyle);
        if Style.CommentaryNewLine then
          Inc(CommentaryIndent, Style.CommentaryIndent);
        Inc(LineIndent, Style.LineIndent);
      end;
      tkEndLine:
      begin
        if VarLevel > 0 then
        begin
          InsertNotation(') ' + LineEnding, Style.MoveStyle);
          EndParagraph(LineIndent);
          if Style.CommentaryNewLine then
            Dec(CommentaryIndent, Style.CommentaryIndent);
          Dec(LineIndent, Style.LineIndent);
          if VarLevel <= FLineStyles.Count then
            Style := FLineStyles.Items[VarLevel - 1]^;
        end;
        BeginParagraph;
        Dec(VarLevel);
      end;
      tkComment:
      begin
        if Style.CommentaryNewLine then
        begin
          EndParagraph(LineIndent);
          BeginParagraph;
          InsertNotation(LineEnding + Token^.Value, Style.CommentaryStyle);
          EndParagraph(CommentaryIndent);
          InsertNotation(LineEnding, Style.CommentaryStyle);
          BeginParagraph;
          AtStartOfLine := True;
        end
        else
        begin
          InsertNotation(Token^.Value + ' ', Style.CommentaryStyle);
        end;
      end;
      tkNAG: InsertNotation(NAGToStr(StrToInt(Token^.Value)) +
          ' ', Style.NAGStyle);
      tkResult: InsertNotation(Token^.Value, Style.MoveStyle);
    end;
  end;
  AGameNotation.ClearAndFree;
end;

end.
