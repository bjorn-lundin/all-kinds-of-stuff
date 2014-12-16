package tst is
  type bnl is record
    a : integer := 10;
    b : String(1..3) := "Hej";
    c : String(1..1017) := (others => '-');
  end record;
end tst;