import java.util.Scanner;

public class Main {
    public static void main(String[] args) {
        if (new Scanner(System.in).nextBoolean()) {
            System.out.println("lol");
        } else {
            System.out.println("kek" + 22);
        }
        while (new Scanner(System.in).nextInt() >= 0) {
            System.out.print("aaa ");
        }
        System.out.print((2 * 2 > 2) ? "Hello, world!" : "test");
    }
}
