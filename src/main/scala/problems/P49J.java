package problems;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class P49J {
  
  // recursive, collection
  public static List<String> grayC(int n) {
    return n == 0 ? Collections.singletonList("") :
      Stream.of("0", "1")
        .flatMap(d ->
          grayC(n - 1).stream().map(s -> d + s)
        )
        .collect(Collectors.toList());
  }

  // recursive, stream
  public static Stream<String> grayS(int n) {
    return n == 0 ? Stream.of("") :
      Stream.of("0", "1")
        .flatMap(d ->
          grayS(n - 1).map(s -> d + s)
        );
  }

  // iterative, while
  public static Stream<String> gray(int n) {
    Stream<String> st = Stream.of("");
    while (n > 0) {
      st = st.flatMap(s -> Stream.of("0", "1").map(d -> d + s));
      n--;
    }
    return st;
  }

  // iterative, for
  public static Stream<String> gray2(int n, Stream<String> st) {
    for (
      st = Stream.of("");
      n > 0;
      n--, st = st.flatMap(s -> Stream.of(0, 1).map(d -> d + s))
    );
    return st;
  }

  public static Stream<String> grayR(int n, Stream<String> st) {
    return n == 0 ? st :
      grayR(n-1, st.flatMap(s -> Stream.of(0, 1).map(d -> s + d)));
  }

  public static void main(String[] args) {
    var x = grayS(4);
    System.out.println(x.toList());
  }

}
