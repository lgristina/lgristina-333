import java.util.ArrayList;
import java.util.List;

public class lab2{

    public static List<List<Integer>> generateLists(int N, int M){

        List<List<Integer>> lists = new ArrayList<>();

        for(int i = M; i >= 1; i--){
            List<Integer> list = new ArrayList<>();
            for(int j = 0; j < N; j++){
                list.add(i + (M * j));
            }
            lists.add(list);
        }
        return lists;
    }

    public static void main(String[] args){
        int N = 6;
        int M = 14;

        List<List<Integer>> lists = generateLists(N, M);

        for(List<Integer> list : lists){
            System.out.println(list);
        }
    }
}