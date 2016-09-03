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

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, StdCtrls,
  Board, MoveList, Pieces, Game, Types, LCLType, Ply;

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
    ComboBox1: TComboBox;
    Label1: TLabel;
    Memo1: TMemo;
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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    MyGame: TGame;
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
begin
  MyGame := TStandardGame.Create(Board1);
  btBackward.Enabled := False;
  btForward.Enabled := False;
  btInitial.Enabled := False;
  btLast.Enabled := False;
  Board1.PieceDirectory := '../Pieces/';
  Board1.CurrentPosition.SetupInitialPosition;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(MyGame);
end;

procedure TForm1.UpdateButtons;
begin
  btBackward.Enabled := MyGame.CurrentPlyNumber > 0;
  btForward.Enabled := MyGame.CurrentPlyNode.Children.Size > 0;
  btInitial.Enabled := MyGame.PlyTree.Count > 0;
  btLast.Enabled := MyGame.PlyTree.Count > 0;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Memo1.Lines.Clear;
  MyGame.Clear;
  Board1.CurrentPosition.SetupInitialPosition;
  Board1.Invalidate;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Board1.Reversed := not Board1.Reversed;
end;

procedure TForm1.btBackwardClick(Sender: TObject);
begin
  MyGame.GoOneMoveBackward;
  UpdateButtons;
end;

procedure TForm1.btForwardClick(Sender: TObject);
begin
  MyGame.GoOneMoveForward;
  UpdateButtons;
end;

procedure TForm1.Board1MovePlayed(AMove: TMove);
var
  Child: TPlyTreeNode;
begin
  if Board1.CurrentPosition.IsLegal(AMove) then
  begin
    if MyGame.CurrentPlyNode.Children.Size = 0 then
    begin
      // New move at the end entered, play it
      MyGame.AddMove(AMove);
      Memo1.Text := MyGame.Notation;
      UpdateButtons;
    end
    else
    begin
      for Child in MyGame.CurrentPlyNode.Children do
      begin
        // the new move is the same as the old one, so we can play it
        if (AMove.IsEqual(Child.Data.Move)) then
        begin
          btForwardClick(Self);
          Exit;
        end;
      end;
      if Application.MessageBox('Should the existing move be replaced?',
        'Question', MB_YESNO) = idYes then
      begin
        MyGame.ReplaceMainLine(AMove);
        Memo1.Text := MyGame.Notation;
        UpdateButtons;
      end
      else
      if Application.MessageBox('Add as new variation?', 'Question',
        MB_YESNO) = idYes then
      begin
        MyGame.AddMoveAsSideLine(AMove);
        Memo1.Text := MyGame.Notation;
        UpdateButtons;
      end;
    end;
  end
  else
    AMove.Free;
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
    UpdateButtons;
  end;
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
  UpdateButtons;
end;

procedure TForm1.btLastClick(Sender: TObject);
begin
  MyGame.GoToPositionAfterPlyNode(MyGame.GetLastPlyNodeInCurrentVariation);
  UpdateButtons;
end;

end.
