{ Demo for Jan's Chess Components - This file contains the main form
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

unit MainForm;

{$mode objfpc}{$H+}

//{$DEFINE LOGGING}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, StdCtrls, Board,
  NotationMemo, MoveList, Pieces, Game, Types, LCLType, ComCtrls, Dialogs,
  Grids, Ply, Position, PGNDbase, PGNGame, BitBoard, NotationToken, LazUTF8,
  RichMemo;

type

  { TForm1 }

  TForm1 = class(TForm)
    Board1: TBoard;
    Button1: TButton;
    Button2: TButton;
    btBackward: TButton;
    btForward: TButton;
    btInitial: TButton;
    btLast: TButton;
    Button3: TButton;
    Button4: TButton;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Memo1: TMemo;
    NotationMemo1: TNotationMemo;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    StringGrid1: TStringGrid;
    procedure Board1HighLightSquare(const SquareCanvas: TCanvas;
      const VisibleRect: TRect; IsWhite: boolean);
    procedure Board1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure Board1MovePlayed(AMove: TMove);
    procedure Board1Promotion(var PromotionPiece: TPieceType);
    procedure btInitialClick(Sender: TObject);
    procedure btLastClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure btBackwardClick(Sender: TObject);
    procedure btForwardClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure NotationMemo1Enter(Sender: TObject);
    procedure NotationMemo1MouseOverToken(Sender: TObject; Token: TNotationToken);
    procedure NotationMemo1ClickMove(Sender: TObject; AMove: TMove);
    procedure NotationMemo1ClickComment(Sender: TObject; var CommentText: string);
    procedure NotationMemo1SelectionChange(Sender: TObject);
    procedure StringGrid1SelectCell(Sender: TObject; aCol, aRow: integer;
      var CanSelect: boolean);
  private
    MyGame: TGame;
    MyPGNGame: TPGNGame;
    PGNDatabase: TPGNDatabase;
    SelLength, SelStart: integer;
    procedure UpdateButtons;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  FEN: string;
begin
  PGNDatabase := TPGNDatabase.Create(True);
  btBackward.Enabled := False;
  btForward.Enabled := False;
  btInitial.Enabled := False;
  btLast.Enabled := False;
  {$IFDEF WINDOWS}
  Board.PieceDirectory := '..\Pieces\'
  {$ELSE}
  Board1.PieceDirectory := '../Pieces/';
  {$ENDIF}
  Board1.CurrentPosition := TStandardPosition.Create;
  // Board1.CurrentPosition.SetupInitialPosition;
  // FEN := '8/8/4p1p1/2p4p/p1PpkP1P/Pr2rRK1/1P1R2P1/8 w - - 0 42';
  // FEN := '8/5bk1/8/2P1p3/8/1K6/8/8 w - d6 0 1';
  //  FEN := 'rnbqkbnr/p1p1p1p1/8/1p1p1p1p/P1P1P1P1/8/1P1P1P1P/RNBQKBNR w KQkq - 0 5';
  //FEN := 'rnbqkbnr/p5p1/8/1ppPpP1p/P5P1/8/1P1P1P1P/RNBQKBNR w KQkq e6 0 7';
  // FEN := '8/8/8/K7/R3Pppk/8/8/8 b - e3 0 1';
  FEN := 'rnk5/pp1b2P1/3QQ3/8/8/8/8/K w - - 0 1';
  (Board1.CurrentPosition as TStandardPosition).FromFEN(FEN);
  // (Board1.CurrentPosition as TStandardPosition).PrintBoards;
  MyGame := TStandardGame.Create;
  MyGame.InitialPosition.Copy(Board1.CurrentPosition);

  with NotationMemo1.AddLineStyle^ do
  begin
    CommentaryStyle.Color := clGreen;
    CommentaryStyle.Style := [];
    MoveStyle.Color := clBlack;
    MoveStyle.Style := [fsBold];
    NAGStyle.Color := clRed;
    NAGStyle.Style := [];
    NumberStyle.Color := clBlack;
    NumberStyle.Style := [fsBold];
    CommentaryNewLine := True;
    CommentaryIndent := 25;
    NeedsNewLine := False;
    LineIndent := 0;
  end;

  with NotationMemo1.AddLineStyle^ do
  begin
    CommentaryStyle.Color := clGreen;
    CommentaryStyle.Style := [];
    MoveStyle.Color := clBlue;
    MoveStyle.Style := [];
    NAGStyle.Color := clBlue;
    NAGStyle.Style := [];
    NumberStyle.Color := clBlue;
    NumberStyle.Style := [];
    CommentaryNewLine := False;
    NeedsNewLine := True;
    LineIndent := 25;
  end;
  with NotationMemo1.MoveToStrOptions do
  begin
    PieceLetters := PieceLetters_DE;
    ShowPawnLetter := True;
    ShowEnPassantSuffix := True;
    PromotionSymbol := psNone;
    CaptureSymbol := csx;
  end;
  SelStart := 0;
  SelLength := 0;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  {$IFDEF Logging}
  ET.Free;
  {$ENDIF}
  FreeAndNil(MyGame);
  PGNDatabase.Free;
end;

procedure TForm1.NotationMemo1Enter(Sender: TObject);
begin
  // never allow focus on NotationMemo1
  ActiveControl := Board1;
end;

procedure TForm1.NotationMemo1MouseOverToken(Sender: TObject; Token: TNotationToken);
begin
  if Assigned(Token) then
  begin
    NotationMemo1.Cursor := crHandPoint;
  end
  else
    NotationMemo1.Cursor := crDefault;
end;

procedure TForm1.NotationMemo1ClickMove(Sender: TObject; AMove: TMove);
begin
  MyPGNGame.GoToPositionAfterMove(AMove);
  NotationMemo1.HighlightMove(AMove);
  Board1.CurrentPosition.Copy(MyPGNGame.CurrentPosition);
  Board1.Invalidate;
end;

procedure TForm1.NotationMemo1ClickComment(Sender: TObject;
  var CommentText: string);
begin
  Memo1.Text := CommentText;
end;

procedure TForm1.NotationMemo1SelectionChange(Sender: TObject);
begin
  // Don't allow any selection
  if NotationMemo1.SelLength > 0 then
    NotationMemo1.SelLength := 0;
end;

procedure TForm1.StringGrid1SelectCell(Sender: TObject; aCol, aRow: integer;
  var CanSelect: boolean);
begin
  if aRow > 0 then
  begin
    MyPGNGame := PGNDatabase.Items[StrToInt(StringGrid1.Cells[7, aRow]) - 1] as TPGNGame;
    // WriteLn(MyPGNGame.GetPGNNotation); // DEBUG
    NotationMemo1.SetTextFromGame(MyPGNGame);
    MyPGNGame.GoToPositionAfterPlyNode(MyPGNGame.PlyTree.Root);
    (Board1.CurrentPosition as TStandardPosition).FromFEN(
      (MyPGNGame.CurrentPosition as TStandardPosition).ToFEN);
    Board1.Invalidate;
  end;
end;

procedure TForm1.UpdateButtons;
begin
  btBackward.Enabled := MyGame.CurrentPlyNumber > 0;
  btForward.Enabled := MyGame.CurrentPlyNode.Children.Size > 0;
  btInitial.Enabled := MyGame.PlyTree.Count > 0;
  btLast.Enabled := MyGame.PlyTree.Count > 0;
  Board1.Invalidate;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  NotationMemo1.Lines.Clear;
  MyGame.Clear;
  Board1.CurrentPosition.SetupInitialPosition;
  UpdateButtons;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Board1.Reversed := not Board1.Reversed;
end;

procedure TForm1.btBackwardClick(Sender: TObject);
begin
  MyGame.GoOneMoveBackward;
  Board1.CurrentPosition.Copy(MyGame.CurrentPosition);
  UpdateButtons;
end;

procedure TForm1.btForwardClick(Sender: TObject);
begin
  MyGame.GoOneMoveForward;
  Board1.CurrentPosition.Copy(MyGame.CurrentPosition);
  UpdateButtons;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  LItem: TListItem;
  i, sum: integer;
  a: extended;
  Temp: TPGNGame;
begin
  a := 0;
  if OpenDialog1.Execute then
  begin
    PGNDatabase.LoadFromFile(OpenDialog1.FileName);
    {$IFDEF Logging}
    a := ET.Elapsed;
    {$ENDIF}
    StringGrid1.BeginUpdate;
    sum := 0;
    for i := 0 to PGNDatabase.Count - 1 do
    begin
      Temp := PGNDatabase.Items[i] as TPGNGame;
      StringGrid1.InsertRowWithValues(StringGrid1.RowCount,
        [Temp.White, Temp.Black, Temp.Date, Temp.Event, Temp.Site,
        Temp.Round, GameResultToStr(Temp.GameResult), IntToStr(i + 1)]);
      sum := sum + PGNDatabase.Items[i].PlyTree.Count div 2;
    end;
    StringGrid1.EndUpdate;
    Label2.Caption := Label2.Caption + FloatToStr(sum / PGNDatabase.Count);
  end;
  {$IFDEF Logging}
  WriteLn('Züge: ', Zuege);
  WriteLn('Zeit: ', Zeit, 's');
  WriteLn(Trunc(Zuege / Zeit), ' Züge pro Sekunde');
  WriteLn(((ET.Elapsed - a)));
  {$ENDIF}
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  if (PGNDatabase.Count > 0) and SaveDialog1.Execute then
  begin
    PGNDatabase.SaveToFile(SaveDialog1.FileName);
  end;
end;

procedure TForm1.Board1MovePlayed(AMove: TMove);
var
  Child: TPlyTreeNode;
begin
  if Board1.CurrentPosition.ValidateMove(AMove) then
  begin
    if MyGame.CurrentPlyNode.Children.Size = 0 then
    begin
      // New move at the end entered, play it
      MyGame.AddMove(AMove);
    end
    else
    begin
      for Child in MyGame.CurrentPlyNode.Children do
      begin
        // the new move is the same as the old one, so we can play it
        if (AMove = Child.Data.Move) then
        begin
          btForwardClick(Self);
          Exit;
        end;
      end;
      if Application.MessageBox('Should the existing move be replaced?',
        'Question', MB_YESNO) = idYes then
      begin
        MyGame.ReplaceMainLine(AMove);
      end
      else
      if Application.MessageBox('Add as new variation?', 'Question',
        MB_YESNO) = idYes then
      begin
        MyGame.AddMoveAsSideLine(AMove);
      end;
    end;
  end
  else
    AMove.Free;
  Board1.CurrentPosition.Copy(MyGame.CurrentPosition);
  NotationMemo1.SetTextFromGame(MyGame);
  NotationMemo1.HighlightMove(MyGame.CurrentPlyNode.Data.Move);
  UpdateButtons;
end;

procedure TForm1.Board1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
var
  Delta, i: integer;
  temp: TPlyTreeNode;
begin
  if MyGame.PlyTree.Count > 0 then
  begin
    Delta := -WheelDelta div 120;
    i := 0;
    if Delta < 0 then // Go back
    begin
      while (i > Delta) and (MyGame.CurrentPlyNode <> MyGame.PlyTree.Root) do
      begin
        MyGame.GoOneMoveBackward;
        Dec(i);
      end;
    end
    else
    if Delta > 0 then // Go forth
    begin
      temp := MyGame.GetLastPlyNodeInCurrentVariation;
      while (i < Delta) and (MyGame.CurrentPlyNode <> temp) do
      begin
        MyGame.GoOneMoveForward;
        Inc(i);
      end;
    end;
    Handled := True;
  end;
end;

procedure TForm1.Board1HighLightSquare(const SquareCanvas: TCanvas;
  const VisibleRect: TRect; IsWhite: boolean);
begin
  SquareCanvas.Pen.Color := clBlue;
  SquareCanvas.Pen.Width := 6;
  SquareCanvas.MoveTo(0, 0);
  SquareCanvas.LineTo(SquareCanvas.Width - 1, 0);
  SquareCanvas.LineTo(SquareCanvas.Width - 1, SquareCanvas.Height - 1);
  SquareCanvas.LineTo(0, SquareCanvas.Height - 1);
  SquareCanvas.LineTo(0, 0);
  //SquareCanvas.FillRect(VisibleRect);
end;

procedure TForm1.Board1Promotion(var PromotionPiece: TPieceType);
begin
  case ComboBox1.ItemIndex of
    0: if Board1.CurrentPosition.WhitesTurn then
        PromotionPiece := ptWQueen
      else
        PromotionPiece := ptBQueen;
    1: if Board1.CurrentPosition.WhitesTurn then
        PromotionPiece := ptWRook
      else
        PromotionPiece := ptBRook;
    2: if Board1.CurrentPosition.WhitesTurn then
        PromotionPiece := ptWKnight
      else
        PromotionPiece := ptBKnight;
    3: if Board1.CurrentPosition.WhitesTurn then
        PromotionPiece := ptWBishop
      else
        PromotionPiece := ptBBishop;
  end;
end;

procedure TForm1.btInitialClick(Sender: TObject);
begin
  MyGame.GoToPositionAfterPlyNode(MyGame.PlyTree.Root);
  Board1.CurrentPosition.Copy(MyGame.CurrentPosition);
  UpdateButtons;
end;

procedure TForm1.btLastClick(Sender: TObject);
begin
  MyGame.GoToPositionAfterPlyNode(MyGame.GetLastPlyNodeInCurrentVariation);
  Board1.CurrentPosition.Copy(MyGame.CurrentPosition);
  UpdateButtons;
end;

end.
