public class Triangle implements Shape {
    private double base;
    private double heightBase;

    public Triangle(double base, double heightBase) {
        this.base = base;
        this.heightBase = heightBase;
    }

    public double calculateArea() {
        return (this.base * this.heightBase) / 2;
    }
}


