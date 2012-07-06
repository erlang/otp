/*
 * %CopyrightBegin%
 *
 * Copyright Scott Lystig Fritchie 2011-2012. All Rights Reserved.
 *
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 *
 * %CopyrightEnd%
 */

/*
 * Purpose:  Dynamically loadable NIF library for DTrace
 */



#include "erl_nif.h"
#include "config.h"
#include "sys.h"
#include "dtrace-wrapper.h"
#if defined(USE_DYNAMIC_TRACE) && (defined(USE_DTRACE) || defined(USE_SYSTEMTAP))
#define HAVE_USE_DTRACE 1
#endif
#ifdef  HAVE_USE_DTRACE
#include "dtrace_user.h"
#endif

void dtrace_nifenv_str(ErlNifEnv *env, char *process_buf);
void get_string_maybe(ErlNifEnv *env, const ERL_NIF_TERM term, char **ptr, char *buf, int bufsiz);

#ifdef VALGRIND
    #  include <valgrind/memcheck.h>
#endif

#ifdef __GNUC__
    #  define INLINE __inline__
#else
    #  define INLINE
#endif

#define MESSAGE_BUFSIZ 1024

/* NIF interface declarations */
static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);

/* The NIFs: */
static ERL_NIF_TERM available(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM user_trace_s1(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM user_trace_i4s4(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM user_trace_n(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] = {
    {"available", 0, available},
    {"user_trace_s1", 1, user_trace_s1},
    {"user_trace_i4s4", 9, user_trace_i4s4},
    {"user_trace_n", 10, user_trace_n}
};

ERL_NIF_INIT(dyntrace, nif_funcs, load, NULL, NULL, NULL)

static ERL_NIF_TERM atom_true;
static ERL_NIF_TERM atom_false;
static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_not_available;
static ERL_NIF_TERM atom_badarg;
static ERL_NIF_TERM atom_ok;

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    atom_true = enif_make_atom(env,"true");
    atom_false = enif_make_atom(env,"false");
    atom_error = enif_make_atom(env,"error");
    atom_not_available = enif_make_atom(env,"not_available");
    atom_badarg = enif_make_atom(env,"badarg");
    atom_ok = enif_make_atom(env,"ok");

    return 0;
}

static ERL_NIF_TERM available(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef	HAVE_USE_DTRACE
    return atom_true;
#else
    return atom_false;
#endif
}

static ERL_NIF_TERM user_trace_s1(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef	HAVE_USE_DTRACE
    ErlNifBinary message_bin;
    DTRACE_CHARBUF(messagebuf, MESSAGE_BUFSIZ + 1);

    if (DTRACE_ENABLED(user_trace_s1)) {
	if (!enif_inspect_iolist_as_binary(env, argv[0], &message_bin) ||
	    message_bin.size > MESSAGE_BUFSIZ) {
	    return atom_badarg;
	}
	memcpy(messagebuf, (char *) message_bin.data, message_bin.size);
        messagebuf[message_bin.size] = '\0';
	DTRACE1(user_trace_s1, messagebuf);
	return atom_true;
    } else {
	return atom_false;
    }
#else
    return atom_error;
#endif
}

void
get_string_maybe(ErlNifEnv *env,
                 const ERL_NIF_TERM term, char **ptr, char *buf, int bufsiz)
{
    ErlNifBinary str_bin;

    if (!enif_inspect_iolist_as_binary(env, term, &str_bin) ||
        str_bin.size > bufsiz) {
        *ptr = NULL;
    } else {
        memcpy(buf, (char *) str_bin.data, str_bin.size);
        buf[str_bin.size] = '\0';
        *ptr = buf;
    }
}

static ERL_NIF_TERM user_trace_i4s4(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef	HAVE_USE_DTRACE
    DTRACE_CHARBUF(procbuf, 32 + 1);
    DTRACE_CHARBUF(user_tagbuf, MESSAGE_BUFSIZ + 1);
    char *utbuf = NULL;
    ErlNifSInt64 i1, i2, i3, i4;
    DTRACE_CHARBUF(messagebuf1, MESSAGE_BUFSIZ + 1);
    DTRACE_CHARBUF(messagebuf2, MESSAGE_BUFSIZ + 1);
    DTRACE_CHARBUF(messagebuf3, MESSAGE_BUFSIZ + 1);
    DTRACE_CHARBUF(messagebuf4, MESSAGE_BUFSIZ + 1);
    char *mbuf1 = NULL, *mbuf2 = NULL, *mbuf3 = NULL, *mbuf4 = NULL;

    if (DTRACE_ENABLED(user_trace_i4s4)) {
	dtrace_nifenv_str(env, procbuf);
        get_string_maybe(env, argv[0], &utbuf, user_tagbuf, MESSAGE_BUFSIZ);
        if (! enif_get_int64(env, argv[1], &i1))
            i1 = 0;
        if (! enif_get_int64(env, argv[2], &i2))
            i2 = 0;
        if (! enif_get_int64(env, argv[3], &i3))
            i3 = 0;
        if (! enif_get_int64(env, argv[4], &i4))
            i4 = 0;
        get_string_maybe(env, argv[5], &mbuf1, messagebuf1, MESSAGE_BUFSIZ);
        get_string_maybe(env, argv[6], &mbuf2, messagebuf2, MESSAGE_BUFSIZ);
        get_string_maybe(env, argv[7], &mbuf3, messagebuf3, MESSAGE_BUFSIZ);
        get_string_maybe(env, argv[8], &mbuf4, messagebuf4, MESSAGE_BUFSIZ);
	DTRACE10(user_trace_i4s4, procbuf, utbuf,
		 i1, i2, i3, i4, mbuf1, mbuf2, mbuf3, mbuf4);
	return atom_true;
    } else {
	return atom_false;
    }
#else
    return atom_error;
#endif
}

#define DTRACE10_LABEL(name, label, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9) \
    erlang_##name##label((a0), (a1), (a2), (a3), (a4), (a5), (a6), (a7), (a8), (a9))
#define N_STATEMENT(the_label) \
   case the_label: \
      if (DTRACE_ENABLED(user_trace_n##the_label)) { \
          dtrace_nifenv_str(env, procbuf); \
          get_string_maybe(env, argv[1], &utbuf, user_tagbuf, MESSAGE_BUFSIZ); \
          if (! enif_get_int64(env, argv[2], &i1)) \
              i1 = 0; \
          if (! enif_get_int64(env, argv[3], &i2)) \
              i2 = 0; \
          if (! enif_get_int64(env, argv[4], &i3)) \
              i3 = 0; \
          if (! enif_get_int64(env, argv[5], &i4)) \
              i4 = 0; \
          get_string_maybe(env, argv[6], &mbuf1, messagebuf1, MESSAGE_BUFSIZ); \
          get_string_maybe(env, argv[7], &mbuf2, messagebuf2, MESSAGE_BUFSIZ); \
          get_string_maybe(env, argv[8], &mbuf3, messagebuf3, MESSAGE_BUFSIZ); \
          get_string_maybe(env, argv[9], &mbuf4, messagebuf4, MESSAGE_BUFSIZ); \
          DTRACE10_LABEL(user_trace_n, the_label, procbuf, utbuf,    \
                         i1, i2, i3, i4, mbuf1, mbuf2, mbuf3, mbuf4); \
          return atom_true; \
      } else { \
          return atom_false; \
      } \
      break

static ERL_NIF_TERM user_trace_n(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef	HAVE_USE_DTRACE
    DTRACE_CHARBUF(procbuf, 32 + 1);
    DTRACE_CHARBUF(user_tagbuf, MESSAGE_BUFSIZ + 1);
    char *utbuf = NULL;
    ErlNifSInt64 i1, i2, i3, i4;
    DTRACE_CHARBUF(messagebuf1, MESSAGE_BUFSIZ + 1);
    DTRACE_CHARBUF(messagebuf2, MESSAGE_BUFSIZ + 1);
    DTRACE_CHARBUF(messagebuf3, MESSAGE_BUFSIZ + 1);
    DTRACE_CHARBUF(messagebuf4, MESSAGE_BUFSIZ + 1);
    char *mbuf1 = NULL, *mbuf2 = NULL, *mbuf3 = NULL, *mbuf4 = NULL;
    ErlNifSInt64 label = 0;

    if (! enif_get_int64(env, argv[0], &label) || label < 0 || label > 1023)
        return atom_badarg;
    switch (label) {
        N_STATEMENT(0);
        N_STATEMENT(1);
        N_STATEMENT(2);
        N_STATEMENT(3);
        N_STATEMENT(4);
        N_STATEMENT(5);
        N_STATEMENT(6);
        N_STATEMENT(7);
        N_STATEMENT(8);
        N_STATEMENT(9);
        N_STATEMENT(10);
        N_STATEMENT(11);
        N_STATEMENT(12);
        N_STATEMENT(13);
        N_STATEMENT(14);
        N_STATEMENT(15);
        N_STATEMENT(16);
        N_STATEMENT(17);
        N_STATEMENT(18);
        N_STATEMENT(19);
        N_STATEMENT(20);
        N_STATEMENT(21);
        N_STATEMENT(22);
        N_STATEMENT(23);
        N_STATEMENT(24);
        N_STATEMENT(25);
        N_STATEMENT(26);
        N_STATEMENT(27);
        N_STATEMENT(28);
        N_STATEMENT(29);
        N_STATEMENT(30);
        N_STATEMENT(31);
        N_STATEMENT(32);
        N_STATEMENT(33);
        N_STATEMENT(34);
        N_STATEMENT(35);
        N_STATEMENT(36);
        N_STATEMENT(37);
        N_STATEMENT(38);
        N_STATEMENT(39);
        N_STATEMENT(40);
        N_STATEMENT(41);
        N_STATEMENT(42);
        N_STATEMENT(43);
        N_STATEMENT(44);
        N_STATEMENT(45);
        N_STATEMENT(46);
        N_STATEMENT(47);
        N_STATEMENT(48);
        N_STATEMENT(49);
        N_STATEMENT(50);
        N_STATEMENT(51);
        N_STATEMENT(52);
        N_STATEMENT(53);
        N_STATEMENT(54);
        N_STATEMENT(55);
        N_STATEMENT(56);
        N_STATEMENT(57);
        N_STATEMENT(58);
        N_STATEMENT(59);
        N_STATEMENT(60);
        N_STATEMENT(61);
        N_STATEMENT(62);
        N_STATEMENT(63);
        N_STATEMENT(64);
        N_STATEMENT(65);
        N_STATEMENT(66);
        N_STATEMENT(67);
        N_STATEMENT(68);
        N_STATEMENT(69);
        N_STATEMENT(70);
        N_STATEMENT(71);
        N_STATEMENT(72);
        N_STATEMENT(73);
        N_STATEMENT(74);
        N_STATEMENT(75);
        N_STATEMENT(76);
        N_STATEMENT(77);
        N_STATEMENT(78);
        N_STATEMENT(79);
        N_STATEMENT(80);
        N_STATEMENT(81);
        N_STATEMENT(82);
        N_STATEMENT(83);
        N_STATEMENT(84);
        N_STATEMENT(85);
        N_STATEMENT(86);
        N_STATEMENT(87);
        N_STATEMENT(88);
        N_STATEMENT(89);
        N_STATEMENT(90);
        N_STATEMENT(91);
        N_STATEMENT(92);
        N_STATEMENT(93);
        N_STATEMENT(94);
        N_STATEMENT(95);
        N_STATEMENT(96);
        N_STATEMENT(97);
        N_STATEMENT(98);
        N_STATEMENT(99);
        N_STATEMENT(100);
        N_STATEMENT(101);
        N_STATEMENT(102);
        N_STATEMENT(103);
        N_STATEMENT(104);
        N_STATEMENT(105);
        N_STATEMENT(106);
        N_STATEMENT(107);
        N_STATEMENT(108);
        N_STATEMENT(109);
        N_STATEMENT(110);
        N_STATEMENT(111);
        N_STATEMENT(112);
        N_STATEMENT(113);
        N_STATEMENT(114);
        N_STATEMENT(115);
        N_STATEMENT(116);
        N_STATEMENT(117);
        N_STATEMENT(118);
        N_STATEMENT(119);
        N_STATEMENT(120);
        N_STATEMENT(121);
        N_STATEMENT(122);
        N_STATEMENT(123);
        N_STATEMENT(124);
        N_STATEMENT(125);
        N_STATEMENT(126);
        N_STATEMENT(127);
        N_STATEMENT(128);
        N_STATEMENT(129);
        N_STATEMENT(130);
        N_STATEMENT(131);
        N_STATEMENT(132);
        N_STATEMENT(133);
        N_STATEMENT(134);
        N_STATEMENT(135);
        N_STATEMENT(136);
        N_STATEMENT(137);
        N_STATEMENT(138);
        N_STATEMENT(139);
        N_STATEMENT(140);
        N_STATEMENT(141);
        N_STATEMENT(142);
        N_STATEMENT(143);
        N_STATEMENT(144);
        N_STATEMENT(145);
        N_STATEMENT(146);
        N_STATEMENT(147);
        N_STATEMENT(148);
        N_STATEMENT(149);
        N_STATEMENT(150);
        N_STATEMENT(151);
        N_STATEMENT(152);
        N_STATEMENT(153);
        N_STATEMENT(154);
        N_STATEMENT(155);
        N_STATEMENT(156);
        N_STATEMENT(157);
        N_STATEMENT(158);
        N_STATEMENT(159);
        N_STATEMENT(160);
        N_STATEMENT(161);
        N_STATEMENT(162);
        N_STATEMENT(163);
        N_STATEMENT(164);
        N_STATEMENT(165);
        N_STATEMENT(166);
        N_STATEMENT(167);
        N_STATEMENT(168);
        N_STATEMENT(169);
        N_STATEMENT(170);
        N_STATEMENT(171);
        N_STATEMENT(172);
        N_STATEMENT(173);
        N_STATEMENT(174);
        N_STATEMENT(175);
        N_STATEMENT(176);
        N_STATEMENT(177);
        N_STATEMENT(178);
        N_STATEMENT(179);
        N_STATEMENT(180);
        N_STATEMENT(181);
        N_STATEMENT(182);
        N_STATEMENT(183);
        N_STATEMENT(184);
        N_STATEMENT(185);
        N_STATEMENT(186);
        N_STATEMENT(187);
        N_STATEMENT(188);
        N_STATEMENT(189);
        N_STATEMENT(190);
        N_STATEMENT(191);
        N_STATEMENT(192);
        N_STATEMENT(193);
        N_STATEMENT(194);
        N_STATEMENT(195);
        N_STATEMENT(196);
        N_STATEMENT(197);
        N_STATEMENT(198);
        N_STATEMENT(199);
        N_STATEMENT(200);
        N_STATEMENT(201);
        N_STATEMENT(202);
        N_STATEMENT(203);
        N_STATEMENT(204);
        N_STATEMENT(205);
        N_STATEMENT(206);
        N_STATEMENT(207);
        N_STATEMENT(208);
        N_STATEMENT(209);
        N_STATEMENT(210);
        N_STATEMENT(211);
        N_STATEMENT(212);
        N_STATEMENT(213);
        N_STATEMENT(214);
        N_STATEMENT(215);
        N_STATEMENT(216);
        N_STATEMENT(217);
        N_STATEMENT(218);
        N_STATEMENT(219);
        N_STATEMENT(220);
        N_STATEMENT(221);
        N_STATEMENT(222);
        N_STATEMENT(223);
        N_STATEMENT(224);
        N_STATEMENT(225);
        N_STATEMENT(226);
        N_STATEMENT(227);
        N_STATEMENT(228);
        N_STATEMENT(229);
        N_STATEMENT(230);
        N_STATEMENT(231);
        N_STATEMENT(232);
        N_STATEMENT(233);
        N_STATEMENT(234);
        N_STATEMENT(235);
        N_STATEMENT(236);
        N_STATEMENT(237);
        N_STATEMENT(238);
        N_STATEMENT(239);
        N_STATEMENT(240);
        N_STATEMENT(241);
        N_STATEMENT(242);
        N_STATEMENT(243);
        N_STATEMENT(244);
        N_STATEMENT(245);
        N_STATEMENT(246);
        N_STATEMENT(247);
        N_STATEMENT(248);
        N_STATEMENT(249);
        N_STATEMENT(250);
        N_STATEMENT(251);
        N_STATEMENT(252);
        N_STATEMENT(253);
        N_STATEMENT(254);
        N_STATEMENT(255);
        N_STATEMENT(256);
        N_STATEMENT(257);
        N_STATEMENT(258);
        N_STATEMENT(259);
        N_STATEMENT(260);
        N_STATEMENT(261);
        N_STATEMENT(262);
        N_STATEMENT(263);
        N_STATEMENT(264);
        N_STATEMENT(265);
        N_STATEMENT(266);
        N_STATEMENT(267);
        N_STATEMENT(268);
        N_STATEMENT(269);
        N_STATEMENT(270);
        N_STATEMENT(271);
        N_STATEMENT(272);
        N_STATEMENT(273);
        N_STATEMENT(274);
        N_STATEMENT(275);
        N_STATEMENT(276);
        N_STATEMENT(277);
        N_STATEMENT(278);
        N_STATEMENT(279);
        N_STATEMENT(280);
        N_STATEMENT(281);
        N_STATEMENT(282);
        N_STATEMENT(283);
        N_STATEMENT(284);
        N_STATEMENT(285);
        N_STATEMENT(286);
        N_STATEMENT(287);
        N_STATEMENT(288);
        N_STATEMENT(289);
        N_STATEMENT(290);
        N_STATEMENT(291);
        N_STATEMENT(292);
        N_STATEMENT(293);
        N_STATEMENT(294);
        N_STATEMENT(295);
        N_STATEMENT(296);
        N_STATEMENT(297);
        N_STATEMENT(298);
        N_STATEMENT(299);
        N_STATEMENT(300);
        N_STATEMENT(301);
        N_STATEMENT(302);
        N_STATEMENT(303);
        N_STATEMENT(304);
        N_STATEMENT(305);
        N_STATEMENT(306);
        N_STATEMENT(307);
        N_STATEMENT(308);
        N_STATEMENT(309);
        N_STATEMENT(310);
        N_STATEMENT(311);
        N_STATEMENT(312);
        N_STATEMENT(313);
        N_STATEMENT(314);
        N_STATEMENT(315);
        N_STATEMENT(316);
        N_STATEMENT(317);
        N_STATEMENT(318);
        N_STATEMENT(319);
        N_STATEMENT(320);
        N_STATEMENT(321);
        N_STATEMENT(322);
        N_STATEMENT(323);
        N_STATEMENT(324);
        N_STATEMENT(325);
        N_STATEMENT(326);
        N_STATEMENT(327);
        N_STATEMENT(328);
        N_STATEMENT(329);
        N_STATEMENT(330);
        N_STATEMENT(331);
        N_STATEMENT(332);
        N_STATEMENT(333);
        N_STATEMENT(334);
        N_STATEMENT(335);
        N_STATEMENT(336);
        N_STATEMENT(337);
        N_STATEMENT(338);
        N_STATEMENT(339);
        N_STATEMENT(340);
        N_STATEMENT(341);
        N_STATEMENT(342);
        N_STATEMENT(343);
        N_STATEMENT(344);
        N_STATEMENT(345);
        N_STATEMENT(346);
        N_STATEMENT(347);
        N_STATEMENT(348);
        N_STATEMENT(349);
        N_STATEMENT(350);
        N_STATEMENT(351);
        N_STATEMENT(352);
        N_STATEMENT(353);
        N_STATEMENT(354);
        N_STATEMENT(355);
        N_STATEMENT(356);
        N_STATEMENT(357);
        N_STATEMENT(358);
        N_STATEMENT(359);
        N_STATEMENT(360);
        N_STATEMENT(361);
        N_STATEMENT(362);
        N_STATEMENT(363);
        N_STATEMENT(364);
        N_STATEMENT(365);
        N_STATEMENT(366);
        N_STATEMENT(367);
        N_STATEMENT(368);
        N_STATEMENT(369);
        N_STATEMENT(370);
        N_STATEMENT(371);
        N_STATEMENT(372);
        N_STATEMENT(373);
        N_STATEMENT(374);
        N_STATEMENT(375);
        N_STATEMENT(376);
        N_STATEMENT(377);
        N_STATEMENT(378);
        N_STATEMENT(379);
        N_STATEMENT(380);
        N_STATEMENT(381);
        N_STATEMENT(382);
        N_STATEMENT(383);
        N_STATEMENT(384);
        N_STATEMENT(385);
        N_STATEMENT(386);
        N_STATEMENT(387);
        N_STATEMENT(388);
        N_STATEMENT(389);
        N_STATEMENT(390);
        N_STATEMENT(391);
        N_STATEMENT(392);
        N_STATEMENT(393);
        N_STATEMENT(394);
        N_STATEMENT(395);
        N_STATEMENT(396);
        N_STATEMENT(397);
        N_STATEMENT(398);
        N_STATEMENT(399);
        N_STATEMENT(400);
        N_STATEMENT(401);
        N_STATEMENT(402);
        N_STATEMENT(403);
        N_STATEMENT(404);
        N_STATEMENT(405);
        N_STATEMENT(406);
        N_STATEMENT(407);
        N_STATEMENT(408);
        N_STATEMENT(409);
        N_STATEMENT(410);
        N_STATEMENT(411);
        N_STATEMENT(412);
        N_STATEMENT(413);
        N_STATEMENT(414);
        N_STATEMENT(415);
        N_STATEMENT(416);
        N_STATEMENT(417);
        N_STATEMENT(418);
        N_STATEMENT(419);
        N_STATEMENT(420);
        N_STATEMENT(421);
        N_STATEMENT(422);
        N_STATEMENT(423);
        N_STATEMENT(424);
        N_STATEMENT(425);
        N_STATEMENT(426);
        N_STATEMENT(427);
        N_STATEMENT(428);
        N_STATEMENT(429);
        N_STATEMENT(430);
        N_STATEMENT(431);
        N_STATEMENT(432);
        N_STATEMENT(433);
        N_STATEMENT(434);
        N_STATEMENT(435);
        N_STATEMENT(436);
        N_STATEMENT(437);
        N_STATEMENT(438);
        N_STATEMENT(439);
        N_STATEMENT(440);
        N_STATEMENT(441);
        N_STATEMENT(442);
        N_STATEMENT(443);
        N_STATEMENT(444);
        N_STATEMENT(445);
        N_STATEMENT(446);
        N_STATEMENT(447);
        N_STATEMENT(448);
        N_STATEMENT(449);
        N_STATEMENT(450);
        N_STATEMENT(451);
        N_STATEMENT(452);
        N_STATEMENT(453);
        N_STATEMENT(454);
        N_STATEMENT(455);
        N_STATEMENT(456);
        N_STATEMENT(457);
        N_STATEMENT(458);
        N_STATEMENT(459);
        N_STATEMENT(460);
        N_STATEMENT(461);
        N_STATEMENT(462);
        N_STATEMENT(463);
        N_STATEMENT(464);
        N_STATEMENT(465);
        N_STATEMENT(466);
        N_STATEMENT(467);
        N_STATEMENT(468);
        N_STATEMENT(469);
        N_STATEMENT(470);
        N_STATEMENT(471);
        N_STATEMENT(472);
        N_STATEMENT(473);
        N_STATEMENT(474);
        N_STATEMENT(475);
        N_STATEMENT(476);
        N_STATEMENT(477);
        N_STATEMENT(478);
        N_STATEMENT(479);
        N_STATEMENT(480);
        N_STATEMENT(481);
        N_STATEMENT(482);
        N_STATEMENT(483);
        N_STATEMENT(484);
        N_STATEMENT(485);
        N_STATEMENT(486);
        N_STATEMENT(487);
        N_STATEMENT(488);
        N_STATEMENT(489);
        N_STATEMENT(490);
        N_STATEMENT(491);
        N_STATEMENT(492);
        N_STATEMENT(493);
        N_STATEMENT(494);
        N_STATEMENT(495);
        N_STATEMENT(496);
        N_STATEMENT(497);
        N_STATEMENT(498);
        N_STATEMENT(499);
        N_STATEMENT(500);
        N_STATEMENT(501);
        N_STATEMENT(502);
        N_STATEMENT(503);
        N_STATEMENT(504);
        N_STATEMENT(505);
        N_STATEMENT(506);
        N_STATEMENT(507);
        N_STATEMENT(508);
        N_STATEMENT(509);
        N_STATEMENT(510);
        N_STATEMENT(511);
        N_STATEMENT(512);
        N_STATEMENT(513);
        N_STATEMENT(514);
        N_STATEMENT(515);
        N_STATEMENT(516);
        N_STATEMENT(517);
        N_STATEMENT(518);
        N_STATEMENT(519);
        N_STATEMENT(520);
        N_STATEMENT(521);
        N_STATEMENT(522);
        N_STATEMENT(523);
        N_STATEMENT(524);
        N_STATEMENT(525);
        N_STATEMENT(526);
        N_STATEMENT(527);
        N_STATEMENT(528);
        N_STATEMENT(529);
        N_STATEMENT(530);
        N_STATEMENT(531);
        N_STATEMENT(532);
        N_STATEMENT(533);
        N_STATEMENT(534);
        N_STATEMENT(535);
        N_STATEMENT(536);
        N_STATEMENT(537);
        N_STATEMENT(538);
        N_STATEMENT(539);
        N_STATEMENT(540);
        N_STATEMENT(541);
        N_STATEMENT(542);
        N_STATEMENT(543);
        N_STATEMENT(544);
        N_STATEMENT(545);
        N_STATEMENT(546);
        N_STATEMENT(547);
        N_STATEMENT(548);
        N_STATEMENT(549);
        N_STATEMENT(550);
        N_STATEMENT(551);
        N_STATEMENT(552);
        N_STATEMENT(553);
        N_STATEMENT(554);
        N_STATEMENT(555);
        N_STATEMENT(556);
        N_STATEMENT(557);
        N_STATEMENT(558);
        N_STATEMENT(559);
        N_STATEMENT(560);
        N_STATEMENT(561);
        N_STATEMENT(562);
        N_STATEMENT(563);
        N_STATEMENT(564);
        N_STATEMENT(565);
        N_STATEMENT(566);
        N_STATEMENT(567);
        N_STATEMENT(568);
        N_STATEMENT(569);
        N_STATEMENT(570);
        N_STATEMENT(571);
        N_STATEMENT(572);
        N_STATEMENT(573);
        N_STATEMENT(574);
        N_STATEMENT(575);
        N_STATEMENT(576);
        N_STATEMENT(577);
        N_STATEMENT(578);
        N_STATEMENT(579);
        N_STATEMENT(580);
        N_STATEMENT(581);
        N_STATEMENT(582);
        N_STATEMENT(583);
        N_STATEMENT(584);
        N_STATEMENT(585);
        N_STATEMENT(586);
        N_STATEMENT(587);
        N_STATEMENT(588);
        N_STATEMENT(589);
        N_STATEMENT(590);
        N_STATEMENT(591);
        N_STATEMENT(592);
        N_STATEMENT(593);
        N_STATEMENT(594);
        N_STATEMENT(595);
        N_STATEMENT(596);
        N_STATEMENT(597);
        N_STATEMENT(598);
        N_STATEMENT(599);
        N_STATEMENT(600);
        N_STATEMENT(601);
        N_STATEMENT(602);
        N_STATEMENT(603);
        N_STATEMENT(604);
        N_STATEMENT(605);
        N_STATEMENT(606);
        N_STATEMENT(607);
        N_STATEMENT(608);
        N_STATEMENT(609);
        N_STATEMENT(610);
        N_STATEMENT(611);
        N_STATEMENT(612);
        N_STATEMENT(613);
        N_STATEMENT(614);
        N_STATEMENT(615);
        N_STATEMENT(616);
        N_STATEMENT(617);
        N_STATEMENT(618);
        N_STATEMENT(619);
        N_STATEMENT(620);
        N_STATEMENT(621);
        N_STATEMENT(622);
        N_STATEMENT(623);
        N_STATEMENT(624);
        N_STATEMENT(625);
        N_STATEMENT(626);
        N_STATEMENT(627);
        N_STATEMENT(628);
        N_STATEMENT(629);
        N_STATEMENT(630);
        N_STATEMENT(631);
        N_STATEMENT(632);
        N_STATEMENT(633);
        N_STATEMENT(634);
        N_STATEMENT(635);
        N_STATEMENT(636);
        N_STATEMENT(637);
        N_STATEMENT(638);
        N_STATEMENT(639);
        N_STATEMENT(640);
        N_STATEMENT(641);
        N_STATEMENT(642);
        N_STATEMENT(643);
        N_STATEMENT(644);
        N_STATEMENT(645);
        N_STATEMENT(646);
        N_STATEMENT(647);
        N_STATEMENT(648);
        N_STATEMENT(649);
        N_STATEMENT(650);
        N_STATEMENT(651);
        N_STATEMENT(652);
        N_STATEMENT(653);
        N_STATEMENT(654);
        N_STATEMENT(655);
        N_STATEMENT(656);
        N_STATEMENT(657);
        N_STATEMENT(658);
        N_STATEMENT(659);
        N_STATEMENT(660);
        N_STATEMENT(661);
        N_STATEMENT(662);
        N_STATEMENT(663);
        N_STATEMENT(664);
        N_STATEMENT(665);
        N_STATEMENT(666);
        N_STATEMENT(667);
        N_STATEMENT(668);
        N_STATEMENT(669);
        N_STATEMENT(670);
        N_STATEMENT(671);
        N_STATEMENT(672);
        N_STATEMENT(673);
        N_STATEMENT(674);
        N_STATEMENT(675);
        N_STATEMENT(676);
        N_STATEMENT(677);
        N_STATEMENT(678);
        N_STATEMENT(679);
        N_STATEMENT(680);
        N_STATEMENT(681);
        N_STATEMENT(682);
        N_STATEMENT(683);
        N_STATEMENT(684);
        N_STATEMENT(685);
        N_STATEMENT(686);
        N_STATEMENT(687);
        N_STATEMENT(688);
        N_STATEMENT(689);
        N_STATEMENT(690);
        N_STATEMENT(691);
        N_STATEMENT(692);
        N_STATEMENT(693);
        N_STATEMENT(694);
        N_STATEMENT(695);
        N_STATEMENT(696);
        N_STATEMENT(697);
        N_STATEMENT(698);
        N_STATEMENT(699);
        N_STATEMENT(700);
        N_STATEMENT(701);
        N_STATEMENT(702);
        N_STATEMENT(703);
        N_STATEMENT(704);
        N_STATEMENT(705);
        N_STATEMENT(706);
        N_STATEMENT(707);
        N_STATEMENT(708);
        N_STATEMENT(709);
        N_STATEMENT(710);
        N_STATEMENT(711);
        N_STATEMENT(712);
        N_STATEMENT(713);
        N_STATEMENT(714);
        N_STATEMENT(715);
        N_STATEMENT(716);
        N_STATEMENT(717);
        N_STATEMENT(718);
        N_STATEMENT(719);
        N_STATEMENT(720);
        N_STATEMENT(721);
        N_STATEMENT(722);
        N_STATEMENT(723);
        N_STATEMENT(724);
        N_STATEMENT(725);
        N_STATEMENT(726);
        N_STATEMENT(727);
        N_STATEMENT(728);
        N_STATEMENT(729);
        N_STATEMENT(730);
        N_STATEMENT(731);
        N_STATEMENT(732);
        N_STATEMENT(733);
        N_STATEMENT(734);
        N_STATEMENT(735);
        N_STATEMENT(736);
        N_STATEMENT(737);
        N_STATEMENT(738);
        N_STATEMENT(739);
        N_STATEMENT(740);
        N_STATEMENT(741);
        N_STATEMENT(742);
        N_STATEMENT(743);
        N_STATEMENT(744);
        N_STATEMENT(745);
        N_STATEMENT(746);
        N_STATEMENT(747);
        N_STATEMENT(748);
        N_STATEMENT(749);
        N_STATEMENT(750);
        N_STATEMENT(751);
        N_STATEMENT(752);
        N_STATEMENT(753);
        N_STATEMENT(754);
        N_STATEMENT(755);
        N_STATEMENT(756);
        N_STATEMENT(757);
        N_STATEMENT(758);
        N_STATEMENT(759);
        N_STATEMENT(760);
        N_STATEMENT(761);
        N_STATEMENT(762);
        N_STATEMENT(763);
        N_STATEMENT(764);
        N_STATEMENT(765);
        N_STATEMENT(766);
        N_STATEMENT(767);
        N_STATEMENT(768);
        N_STATEMENT(769);
        N_STATEMENT(770);
        N_STATEMENT(771);
        N_STATEMENT(772);
        N_STATEMENT(773);
        N_STATEMENT(774);
        N_STATEMENT(775);
        N_STATEMENT(776);
        N_STATEMENT(777);
        N_STATEMENT(778);
        N_STATEMENT(779);
        N_STATEMENT(780);
        N_STATEMENT(781);
        N_STATEMENT(782);
        N_STATEMENT(783);
        N_STATEMENT(784);
        N_STATEMENT(785);
        N_STATEMENT(786);
        N_STATEMENT(787);
        N_STATEMENT(788);
        N_STATEMENT(789);
        N_STATEMENT(790);
        N_STATEMENT(791);
        N_STATEMENT(792);
        N_STATEMENT(793);
        N_STATEMENT(794);
        N_STATEMENT(795);
        N_STATEMENT(796);
        N_STATEMENT(797);
        N_STATEMENT(798);
        N_STATEMENT(799);
        N_STATEMENT(800);
        N_STATEMENT(801);
        N_STATEMENT(802);
        N_STATEMENT(803);
        N_STATEMENT(804);
        N_STATEMENT(805);
        N_STATEMENT(806);
        N_STATEMENT(807);
        N_STATEMENT(808);
        N_STATEMENT(809);
        N_STATEMENT(810);
        N_STATEMENT(811);
        N_STATEMENT(812);
        N_STATEMENT(813);
        N_STATEMENT(814);
        N_STATEMENT(815);
        N_STATEMENT(816);
        N_STATEMENT(817);
        N_STATEMENT(818);
        N_STATEMENT(819);
        N_STATEMENT(820);
        N_STATEMENT(821);
        N_STATEMENT(822);
        N_STATEMENT(823);
        N_STATEMENT(824);
        N_STATEMENT(825);
        N_STATEMENT(826);
        N_STATEMENT(827);
        N_STATEMENT(828);
        N_STATEMENT(829);
        N_STATEMENT(830);
        N_STATEMENT(831);
        N_STATEMENT(832);
        N_STATEMENT(833);
        N_STATEMENT(834);
        N_STATEMENT(835);
        N_STATEMENT(836);
        N_STATEMENT(837);
        N_STATEMENT(838);
        N_STATEMENT(839);
        N_STATEMENT(840);
        N_STATEMENT(841);
        N_STATEMENT(842);
        N_STATEMENT(843);
        N_STATEMENT(844);
        N_STATEMENT(845);
        N_STATEMENT(846);
        N_STATEMENT(847);
        N_STATEMENT(848);
        N_STATEMENT(849);
        N_STATEMENT(850);
        N_STATEMENT(851);
        N_STATEMENT(852);
        N_STATEMENT(853);
        N_STATEMENT(854);
        N_STATEMENT(855);
        N_STATEMENT(856);
        N_STATEMENT(857);
        N_STATEMENT(858);
        N_STATEMENT(859);
        N_STATEMENT(860);
        N_STATEMENT(861);
        N_STATEMENT(862);
        N_STATEMENT(863);
        N_STATEMENT(864);
        N_STATEMENT(865);
        N_STATEMENT(866);
        N_STATEMENT(867);
        N_STATEMENT(868);
        N_STATEMENT(869);
        N_STATEMENT(870);
        N_STATEMENT(871);
        N_STATEMENT(872);
        N_STATEMENT(873);
        N_STATEMENT(874);
        N_STATEMENT(875);
        N_STATEMENT(876);
        N_STATEMENT(877);
        N_STATEMENT(878);
        N_STATEMENT(879);
        N_STATEMENT(880);
        N_STATEMENT(881);
        N_STATEMENT(882);
        N_STATEMENT(883);
        N_STATEMENT(884);
        N_STATEMENT(885);
        N_STATEMENT(886);
        N_STATEMENT(887);
        N_STATEMENT(888);
        N_STATEMENT(889);
        N_STATEMENT(890);
        N_STATEMENT(891);
        N_STATEMENT(892);
        N_STATEMENT(893);
        N_STATEMENT(894);
        N_STATEMENT(895);
        N_STATEMENT(896);
        N_STATEMENT(897);
        N_STATEMENT(898);
        N_STATEMENT(899);
        N_STATEMENT(900);
        N_STATEMENT(901);
        N_STATEMENT(902);
        N_STATEMENT(903);
        N_STATEMENT(904);
        N_STATEMENT(905);
        N_STATEMENT(906);
        N_STATEMENT(907);
        N_STATEMENT(908);
        N_STATEMENT(909);
        N_STATEMENT(910);
        N_STATEMENT(911);
        N_STATEMENT(912);
        N_STATEMENT(913);
        N_STATEMENT(914);
        N_STATEMENT(915);
        N_STATEMENT(916);
        N_STATEMENT(917);
        N_STATEMENT(918);
        N_STATEMENT(919);
        N_STATEMENT(920);
        N_STATEMENT(921);
        N_STATEMENT(922);
        N_STATEMENT(923);
        N_STATEMENT(924);
        N_STATEMENT(925);
        N_STATEMENT(926);
        N_STATEMENT(927);
        N_STATEMENT(928);
        N_STATEMENT(929);
        N_STATEMENT(930);
        N_STATEMENT(931);
        N_STATEMENT(932);
        N_STATEMENT(933);
        N_STATEMENT(934);
        N_STATEMENT(935);
        N_STATEMENT(936);
        N_STATEMENT(937);
        N_STATEMENT(938);
        N_STATEMENT(939);
        N_STATEMENT(940);
        N_STATEMENT(941);
        N_STATEMENT(942);
        N_STATEMENT(943);
        N_STATEMENT(944);
        N_STATEMENT(945);
        N_STATEMENT(946);
        N_STATEMENT(947);
        N_STATEMENT(948);
        N_STATEMENT(949);
        N_STATEMENT(950);
        N_STATEMENT(951);
        N_STATEMENT(952);
        N_STATEMENT(953);
        N_STATEMENT(954);
        N_STATEMENT(955);
        N_STATEMENT(956);
        N_STATEMENT(957);
        N_STATEMENT(958);
        N_STATEMENT(959);
        N_STATEMENT(960);
        N_STATEMENT(961);
        N_STATEMENT(962);
        N_STATEMENT(963);
        N_STATEMENT(964);
        N_STATEMENT(965);
        N_STATEMENT(966);
        N_STATEMENT(967);
        N_STATEMENT(968);
        N_STATEMENT(969);
        N_STATEMENT(970);
        N_STATEMENT(971);
        N_STATEMENT(972);
        N_STATEMENT(973);
        N_STATEMENT(974);
        N_STATEMENT(975);
        N_STATEMENT(976);
        N_STATEMENT(977);
        N_STATEMENT(978);
        N_STATEMENT(979);
        N_STATEMENT(980);
        N_STATEMENT(981);
        N_STATEMENT(982);
        N_STATEMENT(983);
        N_STATEMENT(984);
        N_STATEMENT(985);
        N_STATEMENT(986);
        N_STATEMENT(987);
        N_STATEMENT(988);
        N_STATEMENT(989);
        N_STATEMENT(990);
        N_STATEMENT(991);
        N_STATEMENT(992);
        N_STATEMENT(993);
        N_STATEMENT(994);
        N_STATEMENT(995);
        N_STATEMENT(996);
        N_STATEMENT(997);
        N_STATEMENT(998);
        N_STATEMENT(999);
        N_STATEMENT(1000);
        N_STATEMENT(1001);
        N_STATEMENT(1002);
        N_STATEMENT(1003);
        N_STATEMENT(1004);
        N_STATEMENT(1005);
        N_STATEMENT(1006);
        N_STATEMENT(1007);
        N_STATEMENT(1008);
        N_STATEMENT(1009);
        N_STATEMENT(1010);
        N_STATEMENT(1011);
        N_STATEMENT(1012);
        N_STATEMENT(1013);
        N_STATEMENT(1014);
        N_STATEMENT(1015);
        N_STATEMENT(1016);
        N_STATEMENT(1017);
        N_STATEMENT(1018);
        N_STATEMENT(1019);
        N_STATEMENT(1020);
        N_STATEMENT(1021);
        N_STATEMENT(1022);
        N_STATEMENT(1023);
    }
    return atom_error;          /* NOTREACHED, shut up the compiler */
#else
    return atom_error;
#endif
}
