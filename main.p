// Comment
fn func() {
    var var1;
    var var2 = "str";
    const const1 = ""; // this cannot be changed.
    return var1;
}

group mygroup {
    @init(name, family) {
        this.name = name;
        this.family = family;
    }
}

$print("Hallo!");