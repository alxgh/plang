for (var i = 0; i <= 2; i = i + 1) {
    if (i == 1) {
        continue;
    }
    print i;
}
print "done";

fn areWeHavingItYet() {
  print "Yes we are!";
}

print areWeHavingItYet;
areWeHavingItYet();
print "yooHoo!";

fn math(x) {
  return x + 11;
}

print math(23);