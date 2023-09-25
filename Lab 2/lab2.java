import java.util.ArrayList;
import java.util.List;

public class lab2{

    // creates the lists using the 2 values taken in and returns a list of the lists
    public static List<List<Integer>> generateLists(int length, int spacer){

        List<List<Integer>> lists = new ArrayList<>();

        // calculates and populates the values in each list
        for(int i = spacer; i >= 1; i--){
            List<Integer> list = new ArrayList<>();
            for(int j = 0; j < length; j++){
                list.add(i + (spacer * j));
            }
            lists.add(list);
        }
        return lists;
    }

    
    public static void main(String[] args){
        
        // Test case for the example on the lab page
        List<List<Integer>> lists = generateLists(6, 14);

        for(List<Integer> list : lists){
            System.out.println(list);
        }
        
        System.out.println();
        System.out.println("-----------------------------------------");
        System.out.println();

        // Another test case with expected values
        List<List<Integer>> lists2 = generateLists(5, 10);

        for(List<Integer> list : lists2){
            System.out.println(list);
        }
    }
}