var a = "global";
{
  fn showA() {
    print a;
  }

  showA();
  a = "salam";
  var a = "block";
  showA();
}