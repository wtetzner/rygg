package org.bovinegenius.cmplang.modules;

import org.bovinegenius.cmplang.ast.module.Ident;
import org.bovinegenius.cmplang.ast.module.Span;
import org.pcollections.PMap;

public class ModuleDef {
    private final PMap<Ident<String, Span>, TypeDef> typeDefs;
    private final PMap<Ident<String, Span>, ValueDef> valueDefs;
    private final PMap<Ident<String, Span>, ModuleDef> moduleDef;

    private ModuleDef(
            final PMap<Ident<String, Span>, TypeDef> typeDefs,
            final PMap<Ident<String, Span>, ValueDef> valueDefs,
            final PMap<Ident<String, Span>, ModuleDef> moduleDef
    ) {
        this.typeDefs = typeDefs;
        this.valueDefs = valueDefs;
        this.moduleDef = moduleDef;
    }



}
