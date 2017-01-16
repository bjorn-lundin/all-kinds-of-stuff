program kinectdemo;

{$mode objfpc}{$H+}

uses
  Interfaces,  Forms, frmmain, EventDispatcherThread, libkinect10, math;

{$R *.res}

begin
  SetexceptionMask([exInvalidOp, exDenormalized, exZeroDivide,
                    exOverflow, exUnderflow, exPrecision]);
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

