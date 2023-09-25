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
        int length = 6;
        int spacer = 14;

        List<List<Integer>> lists = generateLists(length, spacer);

        for(List<Integer> list : lists){
            System.out.println(list);
        }
    }
}