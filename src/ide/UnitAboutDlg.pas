(* Window to show information about the project. *)
UNIT UnitAboutDlg;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Buttons;

TYPE
{ TAboutDialog }
  TAboutDialog = CLASS(TForm)
    ButtonClose: TBitBtn;
    LabelWebPage: TLabel;
    Logo: TImage;
    BottomPanel: TPanel;
    LogoPanel: TPanel;
    AuthorsPanel: TPanel;
  PRIVATE
    { private declarations }
  PUBLIC
    { public declarations }
  END;

VAR
  AboutDialog: TAboutDialog;

IMPLEMENTATION

INITIALIZATION
  {$I UnitAboutDlg.lrs}

END.

