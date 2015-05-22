public class Person {
    private String firstName;
    private String lastName;
    private int age;

    public Person(String firstName, String lastName, int age) {
        this.firstName = firstName;
        this.lastName = lastName;
        this.age = age;
    }

    private void print() {
        System.out.printf("%s %s %d\n",
            this.firstName, this.lastName, this.age);
    }
}


