package c4.ast;

import c4.io.*;
import scala.Option;

import java_cup.runtime.Symbol;

import c4.util.Located;

public class C4Scanner implements java_cup.runtime.Scanner {
    private final SourcePhase7Reader reader;

    public C4Scanner(SourcePhase7Reader reader) {
        this.reader = reader;
    }

    @Override
    public Symbol next_token() throws Exception {
        Option<Located<Tok>> tok = reader.get();
        if (tok.isDefined()) {
            Located<Tok> located = tok.get();
            if (located.value() instanceof TokInteger) {
                TokInteger tokInt = (TokInteger) located.value();
                return new Symbol(CupSymbols.NUMBER, tokInt.n().bigInteger());
            } else if (located.value() instanceof Tok_$plus$) {
                return new Symbol(CupSymbols.PLUS);
            } else if (located.value() instanceof Tok_semicolon$) {
                return new Symbol(CupSymbols.SEMI);
            } else {
                throw new RuntimeException("not supported yet");
            }
        } else {
            return new Symbol(CupSymbols.EOF);
        }
    }
}
