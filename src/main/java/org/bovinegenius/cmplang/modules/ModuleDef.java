package org.bovinegenius.cmplang.modules;

import lombok.Getter;
import org.bovinegenius.cmplang.ast.module.Ident;
import org.bovinegenius.cmplang.ast.module.Span;
import org.pcollections.PMap;

public class ModuleDef {
    @Getter private final Ident<String, Span> name;
    private final PMap<Ident<String, Span>, TypeDef> typeDefs;
    private final PMap<Ident<String, Span>, ValueDef> valueDefs;
    private final PMap<Ident<String, Span>, ModuleDef> moduleDef;

    private ModuleDef(
            final Ident<String, Span> name,
            final PMap<Ident<String, Span>, TypeDef> typeDefs,
            final PMap<Ident<String, Span>, ValueDef> valueDefs,
            final PMap<Ident<String, Span>, ModuleDef> moduleDef
    ) {
        this.name = name;
        this.typeDefs = typeDefs;
        this.valueDefs = valueDefs;
        this.moduleDef = moduleDef;
    }

    public ModuleDef

}
