var a = "global";
print a;
fn c() {
  var a = 13;
  fn showA() {
    print a;
    a = a + 1;
  }
  ret showA;
}

var p = c();

p();