package org.bovinegenius.cmplang.util;

import java.lang.reflect.Constructor;

public class Reflect {
    private static final Void VOID = createVoid();

    private static Void createVoid() {
        try {
            Constructor<Void> voidConstructor = Void.class.getDeclaredConstructor();
            voidConstructor.setAccessible(true);
            return voidConstructor.newInstance();
        } catch (Exception ex) {
            return null;
        }
    }

    public static Void voidValue() {
        return VOID;
    }
}
