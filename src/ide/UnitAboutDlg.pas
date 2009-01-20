(* Window to show information about the project. *)
unit UnitAboutDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Buttons;

type

  { TAboutDialog }

  TAboutDialog = class(TForm)
    ButtonClose: TBitBtn;
    LabelWebPage: TLabel;
    Logo: TImage;
    BottomPanel: TPanel;
    LogoPanel: TPanel;
    AuthorsPanel: TPanel;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  AboutDialog: TAboutDialog;

implementation

initialization
  {$I UnitAboutDlg.lrs}

end.

