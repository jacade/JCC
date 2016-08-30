unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Board, MoveList, Position, Pieces;

type

  { TForm1 }

  TForm1 = class(TForm)
    Board1: TBoard;
    Button1: TButton;
    Button2: TButton;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Memo1: TMemo;
    procedure Board1MovePlayed(AMove: TMove);
    procedure Board1Promotion(var PromotionPiece: TPieceType);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
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
  Board1.PieceDirectory := '../Pieces/';
  Board1.CurrentPosition.SetupInitialPosition;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Memo1.Lines.Clear;
  Board1.CurrentPosition.SetupInitialPosition;
  Board1.Invalidate;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Board1.Reversed := not Board1.Reversed;
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
    Board1.CurrentPosition.PlayMove(AMove);
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
