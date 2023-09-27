var x = 13;
print x;
{
    print x;
    x = 12;
    print x;
    var x = 14;
    print x;
}
print x;
x = 15;
if (x == 12 or x == 15) {
    print "HI!";
} else if (x == 13) {
    print "DOOM!";
} else {
    print "OOPS!";
}

var z = 3;

while (z >= 0) {
    print z;
    z = z - 1;
}


for (p, 10..0:-1) {
    z = z + 1;
}

for (p, 100..1:/2) {
    z = z + 1;
}

print time();

fn mamad() {
    print "MAMADDDD!!!";
}

mamad();

var i = 3;

print i;

fn s() {
    i = i -1;
    print i;
}

s();
s();
s();

fn rec(i) {
    if (i > 0) {
        print "not now";
        rec(i - 1);
    } else {
        print "now";
    }
}

rec(3);

fn calc(i) {
    ret i - 1;
}

print calc(3);