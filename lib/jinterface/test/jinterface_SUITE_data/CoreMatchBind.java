/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2000-2015. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * %CopyrightEnd%
 */

import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangMap;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpOutputStream;

public class CoreMatchBind {

    @SuppressWarnings("serial")
    private static class DumbObject extends OtpErlangObject {

        @Override
        public String toString() {
            return this.getClass().getSimpleName();
        }

        @Override
        public void encode(final OtpOutputStream buf) {
            fail("unexpected encode() call");
        }

        @Override
        public boolean equals(final Object o) {
            fail("unexpected equals() call");
            return false;
        }

    }

    @SuppressWarnings("serial")
    private static class BoundObject extends OtpErlangObject {

        @Override
        public String toString() {
            return this.getClass().getSimpleName();
        }

        @Override
        public void encode(final OtpOutputStream buf) {
            fail("unexpected encode() call");
        }

        @Override
        public boolean equals(final Object o) {
            fail("unexpected equals() call");
            return false;
        }

    }

    @SuppressWarnings("serial")
    private static class TestObject extends OtpErlangObject {

        private final Binder binder;
        private DumbObject dumb;
        private boolean flag;
        private BoundObject obj;

        public TestObject(final boolean flag, final Binder binder,
                final DumbObject dumb) {
            this.flag = flag;
            this.binder = binder;
            this.dumb = dumb;
        }

        public TestObject(final Binder binder, final BoundObject obj) {
            this.binder = binder;
            this.obj = obj;
        }

        public DumbObject getDumb() {
            return dumb;
        }

        @Override
        public String toString() {
            return flag ? "T" : "F";
        }

        @Override
        public void encode(final OtpOutputStream buf) {
            fail("unexpected encode() call");
        }

        @Override
        public boolean equals(final Object o) {
            if (obj == null) {
                fail("unexpected equals() call");
            }
            return o == obj;
        }

        @Override
        public <T> boolean match(final OtpErlangObject term, final T binds) {
            if (binds != binder) {
                fail("invalid binder");
            }
            if (term != dumb) {
                fail("invalid object");
            }
            return flag;
        }

        @Override
        public <T> OtpErlangObject bind(final T binds)
                throws OtpErlangException {
            if (binds != binder) {
                fail("invalid binder");
            }
            return obj;
        }

    }

    /*
     * "always matched" object
     */
    @SuppressWarnings("serial")
    private static class Any extends OtpErlangObject {

        @Override
        public String toString() {
            return "any";
        }

        @Override
        public void encode(final OtpOutputStream buf) {
            fail("unexpected encode() call");
        }

        @Override
        public boolean equals(final Object o) {
            fail("unexpected equals() call");
            return false;
        }

        @Override
        public <T> boolean match(final OtpErlangObject term, final T binds) {
            return true;
        }
    }

    private static class Binder {
        // make object pair for match() testing
        TestObject makeTest(final boolean flag) {
            return new TestObject(flag, this, new DumbObject());
        }

        // make object pair for bind() testing
        TestObject makeTest() {
            return new TestObject(this, new BoundObject());
        }
    }

    private static void isNotNull(final Object o) throws Exception {
        if (o == null) {
            throw new Exception("not null expected");
        }
    }

    private static void fail(final String string) {
        System.err.println(string);
        new Throwable().printStackTrace(System.err);
        System.exit(1);
    }

    private static void isT(final boolean b) throws Exception {
        if (!b) {
            throw new Exception("true expected");
        }
    }

    private static void isF(final boolean b) throws Exception {
        if (b) {
            throw new Exception("false expected");
        }
    }

    private static void equals(final OtpErlangObject a, final OtpErlangObject b)
            throws Exception {
        if (!a.equals(b)) {
            throw new Exception(a + " != " + b);
        }
    }

    /*
     * scalar match test - match particular test object (producing given result)
     * against particular dumb object passing particular bindings object; ensure
     * all participants are used as expected in match behavior, check result.
     */
    private static void scalar_match_test() throws Exception {
        final Binder bind = new Binder();

        final TestObject t = bind.makeTest(true);
        isT(t.match(t.getDumb(), bind));

        final TestObject f = bind.makeTest(false);
        isF(f.match(f.getDumb(), bind));
    }

    /*
     * scalar bind test - ensure right object generated based on bindings
     */
    private static void scalar_bind_test() throws Exception {
        final Binder bind = new Binder();
        final TestObject t = bind.makeTest();
        final OtpErlangObject o = t.bind(bind);
        isNotNull(o);
        equals(t, o);
    }

    /*
     * used by tuple_arity_match_test()
     */
    private static OtpErlangObject mkTuplePattern(final int n) {
        final Any a[] = new Any[n];
        for (int i = 0; i < n; i++) {
            a[i] = new Any();
        }
        return new OtpErlangTuple(a);
    }

    /*
     * used by tuple_arity_match_test()
     */
    private static OtpErlangObject mkTupleObject(final int n) {
        final DumbObject a[] = new DumbObject[n];
        for (int i = 0; i < n; i++) {
            a[i] = new DumbObject();
        }
        return new OtpErlangTuple(a);
    }

    /*
     * ensure only tuples of the same arity can match
     */
    private static void tuple_arity_match_test(final int m, final int n)
            throws Exception {
        final Binder bind = new Binder();
        for (int i = m; i < n; i++) {
            for (int j = m; j < n; j++) {
                final OtpErlangObject p = mkTuplePattern(i);
                final OtpErlangObject o = mkTupleObject(j);
                if (i == j) {
                    isT(p.match(o, bind));
                } else {
                    isF(p.match(o, bind));
                }
            }
        }
    }

    /*
     * tuple match test - ensure elements of tuple are matched to corresponding
     * elements of tested object and result is logical "and" over all elements.
     */
    private static void tuple_match_test(final int n) throws Exception {
        final Binder bind = new Binder();
        final int max = 1 << n;
        final TestObject a[] = new TestObject[n];
        final DumbObject d[] = new DumbObject[n];
        for (int k = 0; k < max; k++) {
            for (int m = 1, i = 0; m < max; m = m << 1, i++) {
                d[i] = new DumbObject();
                a[i] = new TestObject((k & m) != 0, bind, d[i]);
            }
            final OtpErlangObject tpl = new OtpErlangTuple(a);
            final OtpErlangObject obj = new OtpErlangTuple(d);
            if (k + 1 < max) {
                isF(tpl.match(obj, bind));
            } else {
                isT(tpl.match(obj, bind));
            }
        }
    }

    /*
     * tuple bind test - ensure result is a tuple where each element is a result
     * of binding of corresponding pattern element using provided bindings.
     */
    private static void tuple_bind_test(final int n) throws Exception {
        final Binder bind = new Binder();
        final TestObject a[] = new TestObject[n];
        final OtpErlangObject b[] = new OtpErlangObject[n];
        for (int i = 0; i < n; i++) {
            a[i] = bind.makeTest();
            b[i] = a[i].obj;
        }
        final OtpErlangObject t = new OtpErlangTuple(a);
        final OtpErlangObject o = t.bind(bind);
        isNotNull(o);
        equals(t, o);
    }

    private static OtpErlangObject mkListPattern(final int n, final boolean tail)
            throws OtpErlangException {
        final Any a[] = new Any[n];
        for (int i = 0; i < n; i++) {
            a[i] = new Any();
        }
        return tail ? new OtpErlangList(a, new Any()) : new OtpErlangList(a);
    }

    private static OtpErlangObject mkListObject(final int n, final boolean tail)
            throws OtpErlangException {
        final DumbObject a[] = new DumbObject[n];
        for (int i = 0; i < n; i++) {
            a[i] = new DumbObject();
        }
        return tail ? new OtpErlangList(a, new DumbObject())
                : new OtpErlangList(a);
    }

    /*
     * ensure only lists of the same arity and same tail presence can match
     */
    private static void list_arity_match_test(final int m, final int n)
            throws Exception {
        final Binder bind = new Binder();
        for (int i = m; i < n; i++) {
            for (int j = m; j < n; j++) {
                for (int k = 0; k < 2; k++) {
                    if (i == 0 && k == 1) {
                        continue;
                    }
                    for (int l = 0; l < 2; l++) {
                        if (j == 0 && l == 1) {
                            continue;
                        }
                        final OtpErlangObject p = mkListPattern(i, k == 1);
                        final OtpErlangObject o = mkListObject(j, l == 1);
                        if (i == j && k == l || k == 1 && i <= j) {
                            isT(p.match(o, bind));
                        } else {
                            isF(p.match(o, bind));
                        }
                    }
                }
            }
        }
    }

    /*
     * lists match test - ensure elements of lists are matched to corresponding
     * elements of tested object and result is logical "and" over all elements,
     * count tails as well
     */
    private static void list_match_test(final int n) throws Exception {
        final Binder bind = new Binder();
        final int max = 1 << n;
        final TestObject a[] = new TestObject[n];
        final DumbObject d[] = new DumbObject[n];
        final DumbObject e[] = new DumbObject[n + 1];
        for (int k = 0; k < max; k++) {
            for (int m = 1, i = 0; m < max; m = m << 1, i++) {
                d[i] = new DumbObject();
                e[i] = d[i];
                a[i] = new TestObject((k & m) != 0, bind, d[i]);
            }
            for (int i = n; i < n + 1; i++) {
                e[i] = new DumbObject();
            }
            final OtpErlangObject lst = new OtpErlangList(a);
            final OtpErlangObject obj = new OtpErlangList(d);
            final OtpErlangObject ext = new OtpErlangList(e);
            final OtpErlangObject eTl = new OtpErlangList(e, new DumbObject());

            if (n > 0) {
                final DumbObject dTail = new DumbObject();
                final TestObject tTail = new TestObject(true, bind, dTail);
                final TestObject fTail = new TestObject(false, bind, dTail);
                final OtpErlangObject fTailLst = new OtpErlangList(a, fTail);
                final OtpErlangObject tTailLst = new OtpErlangList(a, tTail);
                final OtpErlangObject tailObj = new OtpErlangList(d, dTail);

                // match lists with non-matching tails is always false
                isF(fTailLst.match(tailObj, bind));

                // match list with no tail to list with tail is always false
                isF(lst.match(tailObj, bind));

                // matching lists with matching tails
                if (k + 1 < max) {
                    isF(tTailLst.match(tailObj, bind));
                } else {
                    isT(tTailLst.match(tailObj, bind));
                }

                // matching shorter pattern with last tail to longer list
                // with or with no extra tail; matching list pattern
                // with last tail to same length list with no tail.
                final Any aTail = new Any();
                final OtpErlangObject shortLst = new OtpErlangList(a, aTail);
                if (k + 1 < max) {
                    isF(shortLst.match(obj, bind)); // same arity
                    isF(shortLst.match(ext, bind)); // pattern arity is less
                    isF(shortLst.match(eTl, bind)); //
                } else {
                    isT(shortLst.match(obj, bind)); // same arity
                    isT(shortLst.match(ext, bind)); // pattern arity is less
                    isT(shortLst.match(eTl, bind)); //
                }
            }

            // matching lists with no tails
            if (k + 1 < max) {
                isF(lst.match(obj, bind));
            } else {
                isT(lst.match(obj, bind));
            }

            // extra-length object, no tail in "pattern"
            isF(lst.match(ext, bind));
        }
    }

    /*
     * list bind test - ensure result is a list where each element is a result
     * of binding of corresponding pattern element using provided bindings.
     */
    private static void list_bind_test(final int n) throws Exception {
        final Binder bind = new Binder();
        final TestObject a[] = new TestObject[n];
        final OtpErlangObject b[] = new OtpErlangObject[n];
        for (int i = 0; i < n; i++) {
            a[i] = bind.makeTest();
            b[i] = a[i].obj;
        }
        OtpErlangObject t = new OtpErlangList(a);
        OtpErlangObject o = t.bind(bind);
        isNotNull(o);
        equals(t, o);
        if (n > 0) {
            // improper list case
            t = new OtpErlangList(a, bind.makeTest());
            o = t.bind(bind);
            isNotNull(o);
            equals(t, o);
        }
    }

    /*
     * map match test - object may have more keys than pattern
     */
    private static void map_match_test(final int m, final int n)
            throws Exception {
        final Binder bind = new Binder();

        // pattern side - m elements
        final OtpErlangObject k1[] = new OtpErlangObject[m];
        final TestObject a[] = new TestObject[m];

        // object side - n elements
        final OtpErlangObject k2[] = new OtpErlangObject[n];
        final DumbObject d[] = new DumbObject[n];

        final int max = Math.max(m, n);
        final int mskHi = 1 << max;
        final int full = (1 << m) - 1;
        for (int k = 0; k < mskHi; k++) {
            for (int msk = 1, i = 0; msk < mskHi; msk = msk << 1, i++) {
                if (i < n) {
                    k2[i] = new OtpErlangInt(i);
                    d[i] = new DumbObject();
                }
                if (i < m) {
                    k1[i] = new OtpErlangInt(i);
                    a[i] = new TestObject((k & msk) != 0, bind, i < n ? d[i]
                            : new DumbObject());
                }
            }
            final OtpErlangObject map = new OtpErlangMap(k1, a); // m items
            final OtpErlangObject obj = new OtpErlangMap(k2, d); // n items
            if ((k & full) == full && m <= n) {
                isT(map.match(obj, bind));
            } else {
                isF(map.match(obj, bind));
            }
        }
    }

    /*
     * map bind test - ensure result is a map where each element is a result of
     * binding of corresponding pattern element using provided bindings.
     */
    private static void map_bind_test(final int n) throws Exception {
        final Binder bind = new Binder();
        final TestObject a[] = new TestObject[n];
        final OtpErlangObject b[] = new OtpErlangObject[n];
        final OtpErlangObject k[] = new OtpErlangObject[n];
        for (int i = 0; i < n; i++) {
            a[i] = bind.makeTest();
            b[i] = a[i].obj;
            k[i] = new OtpErlangInt(i);
        }
        final OtpErlangObject t = new OtpErlangMap(k, a);
        final OtpErlangObject o = t.bind(bind);
        isNotNull(o);
        equals(t, o);
    }

    public static void main(final String[] args) {
        try {
            scalar_match_test();
            System.out.println("scalar_match_test() passed");

            scalar_bind_test();
            System.out.println("scalar_bind_test() passed");

            for (int m = 0; m < 16; m++) {
                for (int n = 0; n < 16; n++) {
                    tuple_arity_match_test(m, n);
                }
            }
            System.out.println("tuple_arity_match_test() passed");

            for (int n = 0; n < 16; n++) {
                tuple_match_test(n);
            }
            System.out.println("tuple_match_test() passed");

            for (int n = 0; n < 16; n++) {
                tuple_bind_test(n);
            }
            System.out.println("tuple_bind_test() passed");

            for (int m = 0; m < 16; m++) {
                for (int n = 0; n < 16; n++) {
                    list_arity_match_test(m, n);
                }
            }
            System.out.println("list_arity_match_test() passed");

            for (int n = 0; n < 16; n++) {
                list_match_test(n);
            }
            System.out.println("list_match_test() passed");

            for (int n = 0; n < 16; n++) {
                list_bind_test(n);
            }
            System.out.println("list_bind_test() passed");

            for (int m = 0; m < 12; m++) {
                for (int n = 0; n < 12; n++) {
                    map_match_test(m, n);
                }
            }
            System.out.println("map_match_test() passed");

            for (int n = 0; n < 16; n++) {
                map_bind_test(n);
            }
            System.out.println("map_bind_test() passed");

        } catch (final Exception e) {
            e.printStackTrace();
            System.exit(1);
        }

        System.out.println("ok");
    }
}
