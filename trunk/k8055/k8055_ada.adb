
with k8055; use k8055;

procedure k8055_ada is
  Device : Longint;
begin
  Device := OpenDevice(0);

  WriteAllDigital(56);
  delay 1.0;

  WriteAllDigital(231);
  delay 1.0;

  ClearDigitalChannel(1);
  delay 1.0;

  WriteAllDigital(0);
  delay 1.0;

  CloseDevice;

end k8055_ada;
