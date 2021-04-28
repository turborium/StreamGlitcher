unit Main;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ExtDlgs, Types;

type

  { TFormMain }

  TFormMain = class(TForm)
    Bevel1: TBevel;
    ButtonSave: TButton;
    ButtonGlitchMe: TButton;
    ButtonLoad: TButton;
    CheckBoxAutoChange: TCheckBox;
    Image: TImage;
    OpenPictureDialog: TOpenPictureDialog;
    PanelControl: TPanel;
    SavePictureDialog: TSavePictureDialog;
    TimerClith: TTimer;
    procedure ButtonGlitchMeClick(Sender: TObject);
    procedure ButtonLoadClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure CheckBoxAutoChangeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ImageMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure TimerClithTimer(Sender: TObject);
  private
    Origin: TBitmap;
    procedure TryGlitch;

    procedure LoadJpegStreamFromBitmap(const JpegStream: TStream;
      const Bitmap: TBitmap; const Quality: TJPEGQualityRange);

    procedure LoadBitmapFromJpegStream(const Bitmap: TBitmap;
      const JpegStream: TStream);

  public

  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

uses
  Math;

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Randomize;

  Origin := TBitmap.Create;
  Origin.Assign(Image.Picture.Bitmap);

  CheckBoxAutoChange.Checked := False;
  TimerClith.Enabled := False;
end;

procedure TFormMain.ButtonGlitchMeClick(Sender: TObject);
begin
  TryGlitch;
end;

procedure TFormMain.ButtonLoadClick(Sender: TObject);
var
  Picture: TPicture;
begin
    if OpenPictureDialog.Execute then
    begin
      Picture := TPicture.Create;
      try
        Picture.LoadFromFile(OpenPictureDialog.FileName);
        Origin.Assign(Picture.Bitmap);
        Image.Picture.Bitmap.Assign(Origin);
      finally
        Picture.Free;
      end;
    end;
end;

procedure TFormMain.ButtonSaveClick(Sender: TObject);
var
  Png: TPortableNetworkGraphic;
begin
  if SavePictureDialog.Execute then
  begin
    Png := TPortableNetworkGraphic.Create;
    try
      Png.Assign(Image.Picture.Bitmap);
      Png.SaveToFile(SavePictureDialog.FileName);
    finally
      Png.Free;
    end;
  end;
end;

procedure TFormMain.CheckBoxAutoChangeChange(Sender: TObject);
begin
  TimerClith.Enabled := CheckBoxAutoChange.Checked;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  Origin.Free;
end;

procedure TFormMain.ImageMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  TryGlitch;
end;

procedure TFormMain.TimerClithTimer(Sender: TObject);
begin
  TryGlitch;
end;

procedure TFormMain.TryGlitch;
var
  I: Integer;
  JpegStream: TBytesStream;
  Quality: Integer;
begin
  JpegStream := TBytesStream.Create;
  try
    while True do
    begin
      if Random(3) = 0 then
        Quality := Random(10) + 1
      else
        Quality := Random(100) + 1;

      LoadJpegStreamFromBitmap(JpegStream, Origin, Quality);

      if Random(2) = 0 then
        JpegStream.Bytes[Min(Random(1000) + 10, JpegStream.Size - 1)] := Random(256);

      for I := 0 to Random(20) + 1 do
      begin
        JpegStream.Bytes[Random(JpegStream.Size)] := Random(256);
      end;

      try
        LoadBitmapFromJpegStream(Image.Picture.Bitmap, JpegStream);
        Break;
      except
        Image.Picture.Assign(Origin);
      end;
    end;
  finally
    JpegStream.Free;
  end;
end;

procedure TFormMain.LoadJpegStreamFromBitmap(const JpegStream: TStream;
  const Bitmap: TBitmap; const Quality: TJPEGQualityRange);
var
  Jpeg: TJPEGImage;
begin
  Jpeg := TJPEGImage.Create;
  try
    Jpeg.Assign(Bitmap);

    JpegStream.Size := 0;

    Jpeg.CompressionQuality := Quality;
    Jpeg.SaveToStream(JpegStream);
  finally
    Jpeg.Free;
  end;
end;

procedure TFormMain.LoadBitmapFromJpegStream(const Bitmap: TBitmap;
  const JpegStream: TStream);
var
  Jpeg: TJPEGImage;
begin
  Jpeg := TJPEGImage.Create;
  try
    JpegStream.Position := 0;
    Jpeg.LoadFromStream(JpegStream);

    Bitmap.Assign(Jpeg);
  finally
    Jpeg.Free;
  end;
end;

end.

