package org.bovinegenius.cmplang.util;

import lombok.EqualsAndHashCode;
import lombok.NonNull;

import java.util.function.Function;

public abstract class Result<V,E> {
    private Result() {}

    public abstract V get();
    public abstract E getErr();

    public abstract boolean isOk();
    public abstract boolean isErr();

    public static <V, E> Result<V, E> ok(V value) {
        return new Ok(value);
    }

    public static <E> Result<Void, E> ok() {
        return new Ok(Reflect.voidValue());
    }

    public static <V, E> Result<V, E> err(E error) {
        return new Err(error);
    }

    public abstract <S> Result<S, E> map(Function<V, S> mapper);
    public abstract <S,X> Result<X, S> mapErr(Function<E, S> mapper);

    @EqualsAndHashCode
    private static class Ok<V, E> extends Result<V, E> {
        private final V ok;

        private Ok(V ok) {
            this.ok = ok;
        }

        @Override
        public V get() {
            return this.ok;
        }

        @Override
        public E getErr() {
            throw new RuntimeException(String.format("Attempted to get error from %s", this));
        }

        @Override
        public boolean isOk() {
            return true;
        }

        @Override
        public boolean isErr() {
            return false;
        }

        @Override
        public <S> Result<S, E> map(@NonNull Function<V, S> mapper) {
            return new Ok(mapper.apply(this.ok));
        }

        @Override
        public <S, X> Result<X, S> mapErr(Function<E, S> mapper) {
            return new Ok(this.ok);
        }

        @Override
        public String toString() {
            return "Ok(" + ok + ")";
        }
    }

    @EqualsAndHashCode
    private static class Err<V, E> extends Result<V, E> {
        private final E err;

        private Err(E err) {
            this.err = err;
        }

        @Override
        public V get() {
            throw new RuntimeException(String.format("Attempted to get value from %s", this));
        }

        @Override
        public E getErr() {
            return this.err;
        }

        @Override
        public boolean isOk() {
            return false;
        }

        @Override
        public boolean isErr() {
            return true;
        }

        @Override
        public <S> Result<S, E> map(Function<V, S> mapper) {
            return new Err(this.err);
        }

        @Override
        public <S, X> Result<X, S> mapErr(Function<E, S> mapper) {
            return new Err(mapper.apply(this.err));
        }

        @Override
        public String toString() {
            return "Err(" + err + ")";
        }
    }
}
