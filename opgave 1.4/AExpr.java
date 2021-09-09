import java.util.Map;
import java.util.HashMap;

abstract class Expr { 
    abstract public int eval(Map<String,Integer> env);
    abstract public Expr Simplify();
    abstract public String toString();

  }

  class CstI extends Expr {
      protected final int i;


      public CstI(int i) {
          this.i = i;
      }

      public int eval(Map<String, Integer> env) {
          return i;
      }

      @Override
      public String toString() {
          return "" + i;
      }

    @Override
    public Expr Simplify() {
        
        return this;
    }

   

  }

  class Var extends Expr { 
  protected final String name;

  public Var(String name) { 
    this.name = name; 
  }

  public int eval(Map<String,Integer> env) {
    return env.get(name);
  } 

  public String toString() {
    return name;
  } 
  @Override
  public Expr Simplify() {
      
      return this;
  }

}

abstract class Binop extends Expr {
    public Expr e1;
    public Expr e2;

    public Binop(Expr e1, Expr e2) {
        this.e1 = e1;
        this.e2 = e2;
    }
}

class Add extends Binop {

    public Add(Expr e1, Expr e2) {
        super(e1, e2);
    }

    @Override
    public int eval(Map<String, Integer> env) {
        return e1.eval(env) + e2.eval(env);
    }

    @Override
    public String toString() {

        return "(" + e1 + " + " + e2 + ")";
    }

   
    
    @Override
    public Expr Simplify() {
        var es1 = e1.Simplify();
        var es2 = e2.Simplify();

        if (es1.toString().equals("0")) {
            return es2;
        } else if (es2.toString().equals("0")) {
            return es1;
        }
        return new Add(es1, es2);
     }
    }

class Sub extends Binop {
    public Sub(Expr e1, Expr e2) {
        super(e1, e2);
    }

    @Override
    public int eval(Map<String, Integer> env) {
        return e1.eval(env) - e2.eval(env);
    } 

    @Override
    public String toString() {

        return "(" + e1 + " - " + e2 + ")";
    }

    @Override
    public Expr Simplify() {
        var es1 = e1.Simplify();
        var es2 = e2.Simplify();

        if (e2.toString().equals("0")) { 
            return es1;
       } else if (es1.toString().equals(es2.toString())) {
           return new CstI(0);
       }
       return this;
    }
}

class Mul extends Binop {
    public Mul(Expr e1, Expr e2) {
        super(e1, e2);
    }

    @Override
    public int eval(Map<String, Integer> env) {
        return e1.eval(env) * e2.eval(env);
    }

    @Override
    public String toString() {

        return "(" + e1 + " * " + e2 + ")";
    }

    @Override
    public Expr Simplify() {
      
        var es1 = e1.Simplify();
        var es2 = e2.Simplify();

        if (es1.toString().equals("0")) {
            return new CstI(0);
        } else if (es2.toString().equals("0")) {
            return new CstI(0);
        }

        if (es1.toString().equals("1")) {
            return es2;
        } else if (es2.toString().equals("1")) {
            return es1;
        }
        return new Mul(es1, es2);
    }


}

  class AExpr {
  public static void main(String[] args) {
    Expr e1 = new CstI(17);
    Expr e2 = new Add(new CstI(17), new Var("b"));
    Expr e3 = new Sub(new CstI(17), new Add(new CstI(8), new Var("c")));
    Expr e4 = new Add(new Mul(new Var("b"),new CstI(1)),new Var("a"));
    Map<String,Integer> env0 = new HashMap<String,Integer>();
    env0.put("a", 3);
    env0.put("c", 78);
    env0.put("baf", 666);
    env0.put("b", 111);

    System.out.println("Env: " + env0.toString());

    System.out.println(e1.toString() + " = " + e1.toString() + " = " + e1.eval(env0));
    System.out.println(e2.toString() + " = " + e2.toString() + " = " + e2.eval(env0));
    System.out.println(e3.toString() + " = " + e3.toString() + " = " + e3.eval(env0));
    
    System.out.println("toString: " + e4.Simplify());

  }
}