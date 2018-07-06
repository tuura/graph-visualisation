import java.util.Random;
import java.util.Scanner;

/**
 * Created by Sam Prescott on 05/07/2018.
 */
public class Main {
    private static Random r;
    private static int i;
    private static int layers;
    private static int approxLayers;


    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);
        String out = "";
        r = new Random();

        System.out.println("Enter approximate number of graph layers:");
        approxLayers = in.nextInt();

        while (layers < approxLayers * 0.9 || layers > approxLayers * 1.1) {
            i = 1;
            layers = 0;
            out = r2Or1();
        }
        System.out.println("A graph with " + layers + " layers:");
        System.out.println(out);
    }

    private static String v() {
        return "(Vertex \"" + i++ + "\")";
    }

    private static String o(String a, String b) {
        return "(Overlay " + a + " " + b + ")";
    }

    private static String c(String a, String b) {
        return "(Connect " + a + " " + b + ")";
    }

    private static String r2Or1() {
        String rtn;
        layers++;

        if ((r.nextBoolean() && layers > 1) || layers > approxLayers)
            rtn = v();
        else
            rtn = r2(r2Or1(), r2Or1());

        return rtn;
    }

    private static String r2(String a, String b) {
        String rtn;

        if (r.nextBoolean())
            rtn = o(a, b);
        else
            rtn = c(a, b);

        return rtn;
    }
}
