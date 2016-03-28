
//----------------------------------------------------
// The following code was generated by CUP v0.11b 20150930 (SVN rev 66)
//----------------------------------------------------

package c4.ast;

import java.math.BigInteger;
import java_cup.runtime.*;
import java_cup.runtime.XMLElement;

/** CUP v0.11b 20150930 (SVN rev 66) generated parser.
  */
@SuppressWarnings({"rawtypes"})
public class CupParser extends java_cup.runtime.lr_parser {

 public final Class getSymbolContainer() {
    return CupSymbols.class;
}

  /** Default constructor. */
  @Deprecated
  public CupParser() {super();}

  /** Constructor which sets the default scanner. */
  @Deprecated
  public CupParser(java_cup.runtime.Scanner s) {super(s);}

  /** Constructor which sets the default scanner. */
  public CupParser(java_cup.runtime.Scanner s, java_cup.runtime.SymbolFactory sf) {super(s,sf);}

  /** Production table. */
  protected static final short _production_table[][] = 
    unpackFromStrings(new String[] {
    "\000\005\000\002\002\005\000\002\002\004\000\002\002" +
    "\004\000\002\003\005\000\002\003\003" });

  /** Access to production table. */
  public short[][] production_table() {return _production_table;}

  /** Parse-action table. */
  protected static final short[][] _action_table = 
    unpackFromStrings(new String[] {
    "\000\012\000\004\006\004\001\002\000\006\004\ufffd\005" +
    "\ufffd\001\002\000\006\002\013\006\004\001\002\000\006" +
    "\004\010\005\007\001\002\000\004\006\004\001\002\000" +
    "\006\002\uffff\006\uffff\001\002\000\006\004\ufffe\005\ufffe" +
    "\001\002\000\006\004\014\005\007\001\002\000\004\002" +
    "\000\001\002\000\006\002\001\006\001\001\002" });

  /** Access to parse-action table. */
  public short[][] action_table() {return _action_table;}

  /** <code>reduce_goto</code> table. */
  protected static final short[][] _reduce_table = 
    unpackFromStrings(new String[] {
    "\000\012\000\006\002\004\003\005\001\001\000\002\001" +
    "\001\000\004\003\011\001\001\000\002\001\001\000\004" +
    "\003\010\001\001\000\002\001\001\000\002\001\001\000" +
    "\002\001\001\000\002\001\001\000\002\001\001" });

  /** Access to <code>reduce_goto</code> table. */
  public short[][] reduce_table() {return _reduce_table;}

  /** Instance of action encapsulation class. */
  protected CUP$CupParser$actions action_obj;

  /** Action encapsulation object initializer. */
  protected void init_actions()
    {
      action_obj = new CUP$CupParser$actions(this);
    }

  /** Invoke a user supplied parse action. */
  public java_cup.runtime.Symbol do_action(
    int                        act_num,
    java_cup.runtime.lr_parser parser,
    java.util.Stack            stack,
    int                        top)
    throws java.lang.Exception
  {
    /* call code in generated class */
    return action_obj.CUP$CupParser$do_action(act_num, parser, stack, top);
  }

  /** Indicates start state. */
  public int start_state() {return 0;}
  /** Indicates start production. */
  public int start_production() {return 1;}

  /** <code>EOF</code> Symbol index. */
  public int EOF_sym() {return 0;}

  /** <code>error</code> Symbol index. */
  public int error_sym() {return 1;}


  /** User initialization code. */
  public void user_init() throws java.lang.Exception
    {
 /* no need to initialize */ 
    }

  /** Scan to get the next Symbol. */
  public java_cup.runtime.Symbol scan()
    throws java.lang.Exception
    {
 return getScanner().next_token(); 
    }

 /* not needed */ 

/** Cup generated class to encapsulate user supplied action code.*/
@SuppressWarnings({"rawtypes", "unchecked", "unused"})
class CUP$CupParser$actions {
  private final CupParser parser;

  /** Constructor */
  CUP$CupParser$actions(CupParser parser) {
    this.parser = parser;
  }

  /** Method 0 with the actual generated action code for actions 0 to 300. */
  public final java_cup.runtime.Symbol CUP$CupParser$do_action_part00000000(
    int                        CUP$CupParser$act_num,
    java_cup.runtime.lr_parser CUP$CupParser$parser,
    java.util.Stack            CUP$CupParser$stack,
    int                        CUP$CupParser$top)
    throws java.lang.Exception
    {
      /* Symbol object for return from actions */
      java_cup.runtime.Symbol CUP$CupParser$result;

      /* select the action based on the action number */
      switch (CUP$CupParser$act_num)
        {
          /*. . . . . . . . . . . . . . . . . . . .*/
          case 0: // expr_list ::= expr_list expr SEMI 
            {
              Object RESULT =null;
		int eleft = ((java_cup.runtime.Symbol)CUP$CupParser$stack.elementAt(CUP$CupParser$top-1)).left;
		int eright = ((java_cup.runtime.Symbol)CUP$CupParser$stack.elementAt(CUP$CupParser$top-1)).right;
		BigInteger e = (BigInteger)((java_cup.runtime.Symbol) CUP$CupParser$stack.elementAt(CUP$CupParser$top-1)).value;
		 System.out.println(e); 
              CUP$CupParser$result = parser.getSymbolFactory().newSymbol("expr_list",0, ((java_cup.runtime.Symbol)CUP$CupParser$stack.elementAt(CUP$CupParser$top-2)), ((java_cup.runtime.Symbol)CUP$CupParser$stack.peek()), RESULT);
            }
          return CUP$CupParser$result;

          /*. . . . . . . . . . . . . . . . . . . .*/
          case 1: // $START ::= expr_list EOF 
            {
              Object RESULT =null;
		int start_valleft = ((java_cup.runtime.Symbol)CUP$CupParser$stack.elementAt(CUP$CupParser$top-1)).left;
		int start_valright = ((java_cup.runtime.Symbol)CUP$CupParser$stack.elementAt(CUP$CupParser$top-1)).right;
		Object start_val = (Object)((java_cup.runtime.Symbol) CUP$CupParser$stack.elementAt(CUP$CupParser$top-1)).value;
		RESULT = start_val;
              CUP$CupParser$result = parser.getSymbolFactory().newSymbol("$START",0, ((java_cup.runtime.Symbol)CUP$CupParser$stack.elementAt(CUP$CupParser$top-1)), ((java_cup.runtime.Symbol)CUP$CupParser$stack.peek()), RESULT);
            }
          /* ACCEPT */
          CUP$CupParser$parser.done_parsing();
          return CUP$CupParser$result;

          /*. . . . . . . . . . . . . . . . . . . .*/
          case 2: // expr_list ::= expr SEMI 
            {
              Object RESULT =null;
		int eleft = ((java_cup.runtime.Symbol)CUP$CupParser$stack.elementAt(CUP$CupParser$top-1)).left;
		int eright = ((java_cup.runtime.Symbol)CUP$CupParser$stack.elementAt(CUP$CupParser$top-1)).right;
		BigInteger e = (BigInteger)((java_cup.runtime.Symbol) CUP$CupParser$stack.elementAt(CUP$CupParser$top-1)).value;
		 System.out.println(e); 
              CUP$CupParser$result = parser.getSymbolFactory().newSymbol("expr_list",0, ((java_cup.runtime.Symbol)CUP$CupParser$stack.elementAt(CUP$CupParser$top-1)), ((java_cup.runtime.Symbol)CUP$CupParser$stack.peek()), RESULT);
            }
          return CUP$CupParser$result;

          /*. . . . . . . . . . . . . . . . . . . .*/
          case 3: // expr ::= expr PLUS expr 
            {
              BigInteger RESULT =null;
		int e1left = ((java_cup.runtime.Symbol)CUP$CupParser$stack.elementAt(CUP$CupParser$top-2)).left;
		int e1right = ((java_cup.runtime.Symbol)CUP$CupParser$stack.elementAt(CUP$CupParser$top-2)).right;
		BigInteger e1 = (BigInteger)((java_cup.runtime.Symbol) CUP$CupParser$stack.elementAt(CUP$CupParser$top-2)).value;
		int e2left = ((java_cup.runtime.Symbol)CUP$CupParser$stack.peek()).left;
		int e2right = ((java_cup.runtime.Symbol)CUP$CupParser$stack.peek()).right;
		BigInteger e2 = (BigInteger)((java_cup.runtime.Symbol) CUP$CupParser$stack.peek()).value;
		 RESULT = e1.add(e2); 
              CUP$CupParser$result = parser.getSymbolFactory().newSymbol("expr",1, ((java_cup.runtime.Symbol)CUP$CupParser$stack.elementAt(CUP$CupParser$top-2)), ((java_cup.runtime.Symbol)CUP$CupParser$stack.peek()), RESULT);
            }
          return CUP$CupParser$result;

          /*. . . . . . . . . . . . . . . . . . . .*/
          case 4: // expr ::= NUMBER 
            {
              BigInteger RESULT =null;
		int nleft = ((java_cup.runtime.Symbol)CUP$CupParser$stack.peek()).left;
		int nright = ((java_cup.runtime.Symbol)CUP$CupParser$stack.peek()).right;
		BigInteger n = (BigInteger)((java_cup.runtime.Symbol) CUP$CupParser$stack.peek()).value;
		 RESULT = n; 
              CUP$CupParser$result = parser.getSymbolFactory().newSymbol("expr",1, ((java_cup.runtime.Symbol)CUP$CupParser$stack.peek()), ((java_cup.runtime.Symbol)CUP$CupParser$stack.peek()), RESULT);
            }
          return CUP$CupParser$result;

          /* . . . . . .*/
          default:
            throw new Exception(
               "Invalid action number "+CUP$CupParser$act_num+"found in internal parse table");

        }
    } /* end of method */

  /** Method splitting the generated action code into several parts. */
  public final java_cup.runtime.Symbol CUP$CupParser$do_action(
    int                        CUP$CupParser$act_num,
    java_cup.runtime.lr_parser CUP$CupParser$parser,
    java.util.Stack            CUP$CupParser$stack,
    int                        CUP$CupParser$top)
    throws java.lang.Exception
    {
              return CUP$CupParser$do_action_part00000000(
                               CUP$CupParser$act_num,
                               CUP$CupParser$parser,
                               CUP$CupParser$stack,
                               CUP$CupParser$top);
    }
}

}
