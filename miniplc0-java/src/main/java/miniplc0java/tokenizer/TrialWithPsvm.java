package miniplc0java.tokenizer;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Scanner;

public class TrialWithPsvm {
    Scanner scanner;
    ArrayList<String> linesBuffer = new ArrayList<>();
    boolean initialized = false;
    public TrialWithPsvm(Scanner scanner) {
        this.scanner = scanner;
    }
    public void readAll() {
        if (initialized) {
            return;
        }
        while (scanner.hasNext()) {
            linesBuffer.add(scanner.nextLine() + '\n');
        }
        // todo:check read \n?
        initialized = true;
    }


    public static void main(String[] args) throws FileNotFoundException {
        File file = new File("/Users/apple/desktop/hello.txt");
        Scanner sc = new Scanner(file);
//        if (sc == null) {
//            System.out.println("null file");
//        } else {
//            System.out.println("we are right for this test");
//        }

        StringIter it = new StringIter(sc);
        it.readAll();

        System.out.print("currentPos is : ");
        System.out.println(it.currentPos());
        System.out.print("getCurrentChar is : ");
        System.out.println(it.getCurrentChar());
        System.out.print("nextChar is : ");
        System.out.println(it.nextChar());
        System.out.print("currentPos is : ");
        System.out.println(it.currentPos());

        System.out.println("-----------");
        System.out.println(it.getCurrentChar());
        System.out.println(it.peekChar());
        System.out.println("---------------");
        System.out.print("nextChar is : ");
        System.out.println(it.nextChar());
        System.out.print("currentPos is : ");
        System.out.println(it.currentPos());
        System.out.print("nextChar is : ");
        System.out.println(it.nextChar());
        System.out.print("currentPos is : ");
        System.out.println(it.currentPos());
        System.out.println(it.getCurrentChar());
        System.out.println(it.peekChar());
        System.out.print("currentPos is : ");
        System.out.println(it.currentPos());
        System.out.println("--------------------");
        System.out.println(it.seeNextChar());
        System.out.print("previousPos is ");
        System.out.println(it.previousPos());
        System.out.print("currentPos is : ");
        System.out.println(it.currentPos());

    }


}
