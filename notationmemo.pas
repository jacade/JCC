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
  RichMemo, Game, StdCtrls, RichMemoUtils, fgl, LazUTF8, NotationToken, MoveList;

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
  // TODO : Why not use classes??
  PLineStyle = ^TLineStyle;

  TLineStyleList = specialize TFPGList<PLineStyle>;

  TTokenPosition = record
    Start: integer;
    UTF8Length: integer;
  end;

  // Note: Token might be nil, if mouse is over a space char or an empty area
  TMouseOverTokenEvent = procedure(Sender: TObject; Token: TNotationToken) of object;

  TClickMoveToken = procedure(Sender: TObject; AMove: TMove;
    Pos, Len: integer) of object;

  { TNotationMemo }

  TNotationMemo = class(TCustomRichMemo)
  private
    CurrentGameNotation: TGameNotation;
    CurrentToken: TNotationToken;
    CurrentPosition: integer;
    FLineStyles: TLineStyleList;
    FOnClickMove: TClickMoveToken;
    FOnMouseOverToken: TMouseOverTokenEvent;
    TokenLookup: array of TTokenPosition;
    function IndexOfTokenAtPos(const Pos: integer): integer;
    procedure SetOnClickMove(AValue: TClickMoveToken);
    procedure SetOnMouseOverToken(AValue: TMouseOverTokenEvent);
  protected
    procedure Click; override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
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
    property OnClickMove: TClickMoveToken read FOnClickMove write SetOnClickMove;
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
    property OnMouseOverToken: TMouseOverTokenEvent
      read FOnMouseOverToken write SetOnMouseOverToken;
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

function TNotationMemo.IndexOfTokenAtPos(const Pos: integer): integer;
var
  i: integer;
begin
  for i := 0 to Length(TokenLookup) - 1 do
  begin
    if (TokenLookup[i].Start < Pos) and (Pos <= TokenLookup[i].Start +
      TokenLookup[i].UTF8Length) then
    begin
      Result := i;
      Exit;
    end;
  end;
  Result := -1;
end;

procedure TNotationMemo.SetOnClickMove(AValue: TClickMoveToken);
begin
  if FOnClickMove = AValue then
    Exit;
  FOnClickMove := AValue;
end;

procedure TNotationMemo.SetOnMouseOverToken(AValue: TMouseOverTokenEvent);
begin
  if FOnMouseOverToken = AValue then
    Exit;
  FOnMouseOverToken := AValue;
end;

procedure TNotationMemo.Click;
var
  Index, i, Pos, Len: longint;
begin
  inherited Click;
  if Assigned(FOnClickMove) then
  begin
    if Assigned(CurrentToken) then
    begin
      case CurrentToken.GetKind of
        tkMove:
        begin
          if Assigned(FOnClickMove) then
          begin
            // Check if there is a previous move number token
            Index := CurrentGameNotation.IndexOf(CurrentToken);
            if CurrentGameNotation.Items[Index - 1].GetKind = tkMoveNumber then
            begin
              Pos := TokenLookup[Index - 1].Start;
              Len := TokenLookup[Index - 1].UTF8Length + TokenLookup[Index].UTF8Length;
            end
            else
            begin
              Pos := CurrentPosition;
              Len := TokenLookup[Index].UTF8Length;
            end;
            FOnClickMove(Self, CurrentToken.Move, Pos, Len);
          end;
        end;
        tkMoveNumber: // Find next move token
          if Assigned(FOnClickMove) then
          begin
            Index := CurrentGameNotation.IndexOf(CurrentToken);
            for i := Index to CurrentGameNotation.Count - 1 do
              if CurrentGameNotation.Items[i].GetKind = tkMove then
              begin
                FOnClickMove(Self, CurrentGameNotation.Items[i].Move,
                  CurrentPosition, TokenLookup[i].UTF8Length +
                  TokenLookup[Index].UTF8Length);
                Exit;
              end;
          end;
        tkBeginLine: ;
        tkEndLine: ;
        tkComment: ;
        tkNAG: ;
        tkResult: ;
      end;
    end;
  end;
end;

procedure TNotationMemo.MouseMove(Shift: TShiftState; X, Y: integer);
var
  Index: integer;
begin
  inherited MouseMove(Shift, X, Y);
  // If this is too slow in here, it has to be moved in MouseDown or MouseUp
  Index := IndexOfTokenAtPos(CharAtPos(X, Y));
  if Index >= 0 then
  begin
    CurrentToken := CurrentGameNotation.Items[Index];
    CurrentPosition := TokenLookup[Index].Start;
  end
  else
    CurrentToken := nil;
  if Assigned(FOnMouseOverToken) then
  begin
    FOnMouseOverToken(Self, CurrentToken);
  end;
end;

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

  CurrentGameNotation := TGameNotation.Create(True);
  CurrentToken := nil;
  FLineStyles := TLineStyleList.Create;
end;

destructor TNotationMemo.Destroy;
var
  Style: PLineStyle;
begin
  for Style in FLineStyles do
    Dispose(Style);
  FLineStyles.Free;
  CurrentGameNotation.Free;
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
  Token: TNotationToken;
  VarLevel, Start, i: integer;
  Style: TLineStyle;
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
  end;

begin
  AtStartOfLine := False;
  CurrentGameNotation.Free;
  CurrentGameNotation := AGame.GetGameNotation;
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
  SetLength(TokenLookup, CurrentGameNotation.Count);
  for i := 0 to CurrentGameNotation.Count - 1 do
  begin
    Token := CurrentGameNotation.Items[i];
    TokenLookup[i].Start := UTF8Length(Self.Text);
    TokenLookup[i].UTF8Length := UTF8Length(Token.Text);
    case Token.GetKind of
      tkMove:
      begin
        if CurrentGameNotation.Items[i + 1].GetKind <> tkNAG then
          InsertNotation(Token.Text + ' ', Style.MoveStyle)
        else
          InsertNotation(Token.Text, Style.MoveStyle);
        AtStartOfLine := False;
      end;
      tkMoveNumber:
      begin
        InsertNotation(Token.Text, Style.NumberStyle);
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
        if VarLevel > 0 then
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
          InsertNotation(LineEnding + Token.Text, Style.CommentaryStyle);
          EndParagraph(CommentaryIndent);
          InsertNotation(LineEnding, Style.CommentaryStyle);
          BeginParagraph;
          AtStartOfLine := True;
        end
        else
        begin
          InsertNotation(Token.Text + ' ', Style.CommentaryStyle);
        end;
      end;
      tkNAG: InsertNotation(Token.Text + ' ', Style.NAGStyle);
      tkResult: InsertNotation(Token.Text, Style.MoveStyle);
    end;
  end;
end;

end.
