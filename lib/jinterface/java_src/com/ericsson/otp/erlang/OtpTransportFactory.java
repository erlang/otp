/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2015. All Rights Reserved.
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

package com.ericsson.otp.erlang;

import java.io.IOException;
import java.net.InetAddress;

/**
 * Factory class used to create client- and server-side transport instances. One
 * static instance of class implementing this interface is created when program
 * loaded. Default implementation used is {@link OtpSocketTransportFactory}.
 * JInterface user can specify custom transport factory implementing this
 * interface in the following ways:
 * <dl>
 * <dt>defining static class as internal to class holding main() method</dt>
 * <dd>In the systems, where main class can be retrieved with
 * <code>System.getProperty("sun.java.command")</code>, user can define static
 * class <b>OtpErlangSystemTuner</b> internal to the main class, providing at
 * least one static method with the name <b>getOtpTransportFactory</b>, with no
 * parameters, returning object of class implementing
 * <b>OtpTransportFactory</b>, for example:
 * 
 * <pre>
 * 
 * public class MyMainClass {
 * 
 *     public static class OtpErlangSystemTuner {
 *         ...
 *         public static OtpTransportFactory getOtpTransportFactory() {
 *             return new MyTransportFactory();
 *         }
 *     }
 * 
 *     public static class MyTransportFactory implements OtpTransportFactory {
 *         ...
 *     }
 * 
 *     public static void main(String[] args) {
 *         ...
 *     }
 * }
 * 
 * 
 * </pre>
 * 
 * </dd>
 * 
 * <dt>specifying factory class in the system properties</dt>
 * <dd>User-defined transport factory class may be specified via system property
 * <b>OtpTransportFactory</b>, for example:
 * 
 * <pre>
 * 
 * package com.my.company;
 * 
 * public static class MyTransportFactory implements OtpTransportFactory {
 *     ...
 * }
 * </pre>
 * 
 * In such case program may be run with
 * -DOtpTransportFactory=com.my.company.MyTransportFactory, or other way of
 * setting system property <i>before execution of static initializers</i> may be
 * used.</dd>
 * </dl>
 * 
 * @author Dmitriy Kargapolov
 */
public interface OtpTransportFactory {

    /**
     * Create instance of {@link OtpTransport}
     * 
     * @param addr
     *            host name or IP address string
     * @param port
     *            port number
     * @return new socket object
     * @throws IOException
     */
    public abstract OtpTransport createTransport(String addr, int port)
            throws IOException;

    /**
     * Create instance of {@link OtpTransport}
     * 
     * @param addr
     *            peer address
     * @param port
     *            port number
     * @return new socket object
     * @throws IOException
     */
    public abstract OtpTransport createTransport(InetAddress addr, int port)
            throws IOException;

    /**
     * Create instance of {@link OtpServerTransport}
     * 
     * @param port
     *            port number to listen on
     * @return new socket object
     * @throws IOException
     */
    public OtpServerTransport createServerTransport(int port)
            throws IOException;
}
