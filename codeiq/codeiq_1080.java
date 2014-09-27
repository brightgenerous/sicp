
// https://codeiq.jp/ace/joboffer_apli/q1080

import java.util.*;


public class codeiq_1080 {

  public static void main(String[] args) {
    List<Steps> stepsList = new Resolver(args).getStepsList(3, 3);
    for (int i = 0; i < stepsList.size(); i++) {
      System.out.println(String.format("解答 %d", i + 1));
      System.out.println(stepsList.get(i));
    }
  }
}

class Step {

  private final String left;

  private final String right;

  public Step(String left) {
    this(left, "");
  }

  public Step(String left, String right) {
    this.left = normalize(left);
    this.right = normalize(right);
  }

  private static String normalize(String str) {
    char[] cs = str.toCharArray();
    Arrays.sort(cs);
    String ret = "";
    for (char c : cs) {
      ret += c;
    }
    return ret;
  }

  public boolean isLeftEmpty() {
    return left.isEmpty();
  }

  public boolean notExistOrGreaterThanEqual
      (char c1, char c2) {
    return notExistOrGreaterThanEqual(left, c1, c2)
      &&  notExistOrGreaterThanEqual(right, c1, c2);
  }

  private boolean notExistOrGreaterThanEqual
      (String str, char c1, char c2) {
    if (str.isEmpty() || (c1 == c2)
        || (str.indexOf(c1) == -1)) {
      return true;
    }
    int count1 = 0;
    int count2 = 0;
    for (char c : str.toCharArray()) {
      if (c == c1) {
        count1++;
      } else if (c == c2) {
        count2++;
      }
    }
    return count1 >= count2;
  }

  public Step leftToRight(char... cs) {
    String newLeft = left;
    String newRight = right;
    for (char c : cs) {
      if (newLeft.indexOf(c) == -1) {
        return null;
      }
      newLeft = newLeft.replaceFirst(c + "", "");
      newRight += c;
    }
    return new Step(newLeft, newRight);
  }

  public Step rightToLeft(char... cs) {
    String newLeft = left;
    String newRight = right;
    for (char c : cs) {
      if (newRight.indexOf(c) == -1) {
        return null;
      }
      newLeft += c;
      newRight = newRight.replaceFirst(c + "", "");
    }
    return new Step(newLeft, newRight);
  }

  public int hashCode() {
    return left.hashCode() * 37 + right.hashCode();
  }

  public boolean equals(Object other) {
    if (!(other instanceof Step)) {
      return false;
    }
    Step o = (Step) other;
    return left.equals(o.left) && right.equals(o.right);
  }

  public String toString() {
    return left + "/" + right;
  }
}

class Steps {

  private final List<Step> steps = new ArrayList<>();

  public Steps() {
  }

  public Steps(Step... steps) {
    for (Step step : steps) {
      this.steps.add(step);
    }
  }

  public Steps(List<Step> steps) {
    this.steps.addAll(steps);
  }

  public Steps add(Step step) {
    List<Step> newSteps = new ArrayList<>(steps);
    newSteps.add(step);
    return new Steps(newSteps);
  }

  public boolean containsOdd(Step step) {
    int count = 0;
    for (Step st : steps) {
      count++;
      if (st.equals(step) && count % 2 == 1) {
        return true;
      }
    }
    return false;
  }

  public boolean containsEven(Step step) {
    int count = 0;
    for (Step st : steps) {
      count++;
      if (st.equals(step) && count % 2 == 0) {
        return true;
      }
    }
    return false;
  }

  public Step getLast() {
    if (steps.isEmpty()) {
      return null;
    }
    return steps.get(steps.size() - 1);
  }

  public boolean isOddSteps() {
    return steps.size() % 2 == 1;
  }

  public String toString() {
    StringBuilder sb = new StringBuilder();
    for (Step step : steps) {
      sb.append(step).append(System.lineSeparator());
    }
    return sb.toString();
  }
}

class Resolver {

  private static final char S = 'S';

  private static final char T = 'T';

  private static final char[][] patterns = new char[][] {
        { S }, { S, S }, { T }, { T, T }, { S, T }
      };


  private String[] args;

  public Resolver(String[] args) {
    this.args = args;
  }

  public List<Steps> getStepsList(int s, int t) {
    Queue<Steps> queue;
    {
      queue = new LinkedList<>();
      String left = "";
      for (int i = 0; i < s; i++) {
        left += S;
      }
      for (int i = 0; i < t; i++) {
        left += T;
      }
      queue.add(new Steps(new Step(left)));
    }

    List<Steps> ret = new ArrayList<>();
    loop: while (!queue.isEmpty()) {
      Steps steps = queue.poll();
      Step last = steps.getLast(); // must not be null.
      if (steps.isOddSteps()) {
        for (char[] pattern : patterns) {
          Step nextStep = last.leftToRight(pattern);
          if (nextStep != null) {
            if (nextStep.isLeftEmpty()) {
              ret.add(steps.add(nextStep));
            }
            if (nextStep.notExistOrGreaterThanEqual(S, T)
                && !steps.containsEven(nextStep)) {
              queue.offer(steps.add(nextStep));
            }
          }
        }
      } else {
        for (char[] pattern : patterns) {
          Step nextStep = last.rightToLeft(pattern);
          if (nextStep != null) {
            if (nextStep.notExistOrGreaterThanEqual(S, T)
                && !steps.containsOdd(nextStep)) {
              queue.offer(steps.add(nextStep));
            }
          }
        }
      }
    }
    return ret;
  }
}

