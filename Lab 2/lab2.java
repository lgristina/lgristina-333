import java.util.ArrayList;
import java.util.List;

public class lab2{

    // creates the lists using the 2 values taken in and returns the list of the lists
    public static List<List<Integer>> generateLists(int length, int spacer){

        if(!(length >= 0 && length == (int)length) || !(spacer > 0 && spacer == (int)spacer)){
            throw new IllegalArgumentException("Error: Please input a non-negative integer");
        }
        
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
        
        System.out.println();
        System.out.println("-----------------------------------------");
        System.out.println("Test Case 1: example from lab page");
        System.out.println();

        try {
            
            // Test case for the example on the lab page
            List<List<Integer>> lists = generateLists(6, 14);

            for(List<Integer> list : lists){
                System.out.println(list);
            }
        } catch (IllegalArgumentException exception) {
            
            System.out.println(exception.getMessage());
        }
        
        System.out.println();
        System.out.println("-----------------------------------------");
        System.out.println("Test Case 2: More expected values");
        System.out.println();

        try{
            
            // Another test case with expected values
            List<List<Integer>> lists2 = generateLists(5, 10);

            for(List<Integer> list : lists2){
                System.out.println(list);
            }
        }
        catch(IllegalArgumentException exception){
            System.out.println(exception.getMessage());
        }

        System.out.println();
        System.out.println("-----------------------------------------");
        System.out.println("Test Case 3: negative integer handling");
        System.out.println();
 
        try{
            
            // Test case for negative integers
            List<List<Integer>> lists3 = generateLists(-5,6);

            for(List<Integer> list : lists3){
                System.out.println(list);
            }
        }
        catch(IllegalArgumentException exception){
            System.out.println(exception.getMessage());
        }

        System.out.println();
        System.out.println("-----------------------------------------");
        System.out.println("Test Case 4: Wanted to use a character to test my handling"); 
        System.out.println("but it wouldn't compile bc its not an integer obviously"); 
        System.out.println();

        try{
            
            /* 
            Wanted to test for more unexpected inputs (ie. char / String) but obviously it wouldn't 
            compile because the generateLists method takes in only ints as its parameters
            */ 
            List<List<Integer>> lists4 = generateLists(-7, 8);

            for(List<Integer> list : lists4){
                System.out.println(list);
            }
        }
        catch(IllegalArgumentException exception){
            System.out.println(exception.getMessage());
        }

        System.out.println();
        System.out.println("-----------------------------------------");
        System.out.println();
    }
}