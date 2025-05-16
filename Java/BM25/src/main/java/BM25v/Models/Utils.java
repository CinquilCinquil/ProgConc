package BM25v.Models;

import java.util.ArrayList;
import java.util.Arrays;

public class Utils {

    public static <T> T[] create_batch(int i, int n_batches, T[] source) {

        int n_per_batch = source.length/n_batches;

        return Arrays.copyOfRange(source, i * n_per_batch, get_upper_limit(i, n_batches, source.length));
    }

    public static <T> ArrayList<T> create_batch(int i, int n_batches, ArrayList<T> source) {

        int n_per_batch = source.size()/n_batches;

        return new ArrayList<>(source.subList(i * n_per_batch, get_upper_limit(i, n_batches, source.size())));
    }

    private static int get_upper_limit(int i, int n_batches, int load_amount) {
        int n_per_batch = load_amount/n_batches;
        return i == n_batches - 1 ? load_amount : (i + 1) * n_per_batch;
    }
}
