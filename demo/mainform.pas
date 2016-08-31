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
  Board, MoveList, Position, Pieces, Game;

type

  { TForm1 }

  TForm1 = class(TForm)
    Board1: TBoard;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Memo1: TMemo;
    procedure Board1MovePlayed(AMove: TMove);
    procedure Board1Promotion(var PromotionPiece: TPieceType);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    MyGame: TGame;
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
  Board1.PieceDirectory := '../Pieces/';
  Board1.CurrentPosition.SetupInitialPosition;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(MyGame);
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

procedure TForm1.Button3Click(Sender: TObject);
begin
  MyGame.GoOneMoveBackward;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  MyGame.GoOneMoveForward;
end;

procedure TForm1.Board1MovePlayed(AMove: TMove);
begin
  if Board1.CurrentPosition.IsLegal(AMove) then
  begin
    if Board1.CurrentPosition.WhitesTurn then
      Memo1.Text := Memo1.Text + (IntToStr(Board1.CurrentPosition.MoveNumber) +
        '. ' + TStandardPosition(Board1.CurrentPosition).MoveToSAN(AMove))
    else
      Memo1.Text := Memo1.Text + ' ' + TStandardPosition(
        Board1.CurrentPosition).MoveToSAN(AMove) + LineEnding;
    MyGame.AddMove(AMove);
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

end.
