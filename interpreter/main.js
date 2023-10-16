class Polygon {
    #name;
    constructor() {
        this.#name = 'Polygon';
    }

    x() {
        console.log(this.#name);
    }
}

const poly1 = new Polygon();

console.log(poly1.#name);
poly1.x();

console.log(Polygon);