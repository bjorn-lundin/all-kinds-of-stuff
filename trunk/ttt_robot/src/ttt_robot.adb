with Stacktrace;

with Motors;

procedure ttt_robot is
begin
  null;

  exception
    when E: others =>
      Stacktrace.Tracebackinfo(E);

end ttt_robot;

