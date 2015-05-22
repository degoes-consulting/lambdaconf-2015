public class Square implements Shape {
    private double width;
    private double height;

    public Square(double width, double height) {
        this.width = width;
        this.height = height;
    }

    public double calculateArea() {
        return this.width * this.height;
    }
}


