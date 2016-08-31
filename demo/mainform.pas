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
  Board, MoveList, Pieces, Game;

type

  { TForm1 }

  TForm1 = class(TForm)
    Board1: TBoard;
    Button1: TButton;
    Button2: TButton;
    btBackward: TButton;
    btForward: TButton;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Memo1: TMemo;
    procedure Board1MovePlayed(AMove: TMove);
    procedure Board1Promotion(var PromotionPiece: TPieceType);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure btBackwardClick(Sender: TObject);
    procedure btForwardClick(Sender: TObject);
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
  btBackward.Enabled := False;
  btForward.Enabled := False;
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

procedure TForm1.btBackwardClick(Sender: TObject);
begin
  MyGame.GoOneMoveBackward;
  if MyGame.CurrentPlyNumber = 0 then
    btBackward.Enabled := False;
  btForward.Enabled := True;
end;

procedure TForm1.btForwardClick(Sender: TObject);
begin
  MyGame.GoOneMoveForward;
  if MyGame.CurrentPlyNumber = MyGame.PlyList.Count then
    btForward.Enabled := False;
  btBackward.Enabled := True;
end;

procedure TForm1.Board1MovePlayed(AMove: TMove);
begin
  if Board1.CurrentPosition.IsLegal(AMove) then
  begin
    // TODO: Implement variations
    // new move, where already one exists
    if (MyGame.CurrentPlyNumber < MyGame.PlyList.Count) then
    begin
      // the new move is the same as the old one, so we can play it
      if (AMove.IsEqual(MyGame.PlyList.Items[MyGame.CurrentPlyNumber].Move)) then
      begin
        btForwardClick(Self);
      end
      else
      begin
        Application.MessageBox('This feature is not implemented yet!', 'Error');
      end;
    end
    else
      // New move entered, play it
    begin
      if Board1.CurrentPosition.WhitesTurn then
        Memo1.Text := Memo1.Text + (IntToStr(Board1.CurrentPosition.MoveNumber) +
          '. ' + MyGame.MoveToString(AMove))
      else
        Memo1.Text := Memo1.Text + ' ' + MyGame.MoveToString(AMove) + LineEnding;
      MyGame.AddMove(AMove);
      btBackward.Enabled := True;
    end;

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
