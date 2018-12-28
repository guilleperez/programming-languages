;==========================================================
; Guillermo Pérez Trueba A01377162
;==========================================================

(use 'clojure.test)

;==========================================================
(defn count-different-emails
  "Counts the number of different emails in lst."
  [lst]
    (let [
          users (map (fn [email] (take-while #(and (not= % \@) (not= % \+)) email)) lst)
          domains (map (fn [email] (rest (drop-while #(not= % \@) email))) lst)
          unique #{}
          ]
      (->> users
           (map (fn [user] (filter #(not= % \.) user)))
           (map #(apply str (concat %2 %1)) domains)
           (into #{})
           (count)
           )))


;==========================================================
(deftest test-count-different-emails-original-requirements
  (is (= 2 (count-different-emails
             ["two.different.providers@now.here"
              "two.different.providers@nowhere"])))
  (is (= 2 (count-different-emails
             ["1.2.3@testing"
              "testing@1.2.3"])))  
  (is (= 5 (count-different-emails
             ["alice@e.mail"
              "eve@another.mail"
              "bob@e.mail"
              "joe90@e.mail"
              "b.o.b@e.mail"
              "bob+new@e.mail"
              "bob@another.provider"])))
  (is (= 52 (count-different-emails
              ["3k7gysj2fzms8753g2614u0j@j.y.w.tvbufj.1v8fs.5n9k6.3j5n.b9.5h.zwdwq24q3p8.vu"
               "v9tqsa5qzf8y8c9698325b5g@vbp3m"
               "e2.h.iu+bkkh5h6b.5.sl@ktj6mf.o94m7pxw.n.5wut9vnb8cvn87h87f0o7s"
               "6y6iq5b4iu4wy5mr4@7r8l9c.v43.l8ln6t5cbtvf.s"
               "cdcrg1a3a7hfy3wmj8cfchhikhz8l8w6tsg7n40v@zqa9lpg5fetjaxqy33xj2sa7bco.uxtbye3rw7da929"
               "2@zofera.vd.1y.cav.1yp.j7.2"
               "dmrv4r2nyi@i9s.g3neiz3ircfma3z.j7z"
               "vt0h5@jr0.kwaz.171.j717gahyh.ob.8ah.w.wnv4.z"
               "w3867l2sqpmx3zir13gh6ya1ibos9f0fd@mo.emctn5woqam"
               "o.w.3.4.j.5v.n.w.w.5.g.d.a.e.3.q.k.u.qn.g.4x.4.m.h.c.4.b.b.q.c+viuy@me.0y.q.k.q.lr.6r.m.65.5.2dly.21"
               "v.t0h5@jr0.kwaz.171.j717gahyh.ob.8ah.w.wnv4.z"
               "e.2.h.i.u+9.j9v@ktj6mf.o94m7pxw.n.5wut9vnb8cvn87h87f0o7s"
               "m.6.u.e.x.e.a.p.g.8.9i.p.qb.y.2.q.vv.c.2.g.e.x.1.9.5.t.r.c.4.j.5.i.a.a.ag.u@hnqwhmv6i"
               "bcsiw04gzzjx38r+v.c.iohch3.0r7uj.e@wdi.eed9akz.ud.r.vd1.ngyq9h.c.t.my.we5rx.0"
               "h.b7.q.6n.3.i.b@gba2cx7havgsd05"
               "exnkb2wtl.zvx3.isp8.ddteovt4jz5k4q@c.6slt.qg3zv.ms.h.8d.w95jbz9.3.l.cwhr.x.a6.g8qowc"
               "uie.xaycgcbt.n3.5.vr.1pr85gw2.fe.ti.4.xc.v3.lcrg9.qa.cq@lx5.xxcujr"
               "e9d3o77oocxjhxsgwjtuv2vo.22civrz.dhqwxyl9y.kq+1@s.p.2lyht.c.kq.ko.yt5.e.0.xk8.h.b.o.6.g.t.oi6"
               "g46bpggtjnavz5mdsft3xwmiird5u2kyzp8bc126@tnr"
               "an.yi.sc6z7j+15k.1b.6.n.de.l.6.k.y.d.m.kg.1.gr.y.6.l.b.x.v.u.mi.y.5.a.ke.h@foo"
               "w2p01utp@7e.1.a"
               "ow34j5vnww5gdae3qkuqng4x4mhc4bbqc@me.0y.q.k.q.lr.6r.m.65.5.2dly.21"
               "uiexaycgcbtn35vr1pr85gw2feti4xcv3lcrg9qacq@3.r0.s.r6nrsnp49.w.60.5ux.qs62f"
               "r.3.3irt3.13.q@tim1xr1x.y.y5rq.i.v8za.m.orq69.gf.974s"
               "d3erf3bavokax599lw7eeoi6sd8ww6yupu5c@nm.8z.84.htvf.sz.u1.s.2.t.7"
               "6f@h.o.ssp"
               "n.d.h.z.6.b+uxehaaatgv3a698d3l5wgpbhtafjd3pmmnovwktcg8@h.5.y.d.t.p.r6.z.f.4p.t.5.9.n.4.0.9.29.m.o.j"
               "r33irt313q@tim1xr1x.y.y5rq.i.v8za.m.orq69.gf.974s"
               "dmrv4r2nyi@au.n0jmq.5sptfw74.t.xmh3.7.gc"
               "r33.irt.3.13q+x6.y.b.g.4.9.x8.51.9.7x.i7.l.gl.6s.j9v.3z.z.ms@tim1xr1x.y.y5rq.i.v8za.m.orq69.gf.974s"
               "xtbymypp.erop.31.0o.dd.d.a.5.8ildc.k20p.q0e.9e0f.p@i.6.r6b.0.4b92bp7jgphh.vq1b"
               "e9d3o77oocxjhxsgwjtuv2vo22civrzdhqwxyl9ykq@s.p.2lyht.c.kq.ko.yt5.e.0.xk8.h.b.o.6.g.t.oi6"
               "r.33.ir.t.3.1.3.q+ch3.l4tx.hhq.hm.s2zl0ji.i.xlud6.ow70@tim1xr1x.y.y5rq.i.v8za.m.orq69.gf.974s"
               "6.f+5xr7684ux2fuzclsjw.g.jo@h.o.ssp"
               "t@6z9l9iff.sb8o5kp5y.oifmkjcswi4v"
               "xkdsu2q8hf338kxqfmeqi@me.0y.q.k.q.lr.6r.m.65.5.2dly.21"
               "cic8mmltwd8svqxeb4dkwl@me.0y.q.k.q.lr.6r.m.65.5.2dly.21"
               "v9.t.qsa.5.qz.f.8.y.8.c9.6.9.8.3.2.5.b.5.g+rxt5bpmhzm5amur30v.5tr.l0.fk.znbvlvtnhumq4wt3dx@vbp3m"
               "vvt6.vp.r.j7avn5.v25ekl7z81.9.v.dhg0@fs.emlq.l.d5s.fxi.c.jdm1e.fq.5lgy.3.i.gs.6"
               "gc4mtw4u@ktj6mf.o94m7pxw.n.5wut9vnb8cvn87h87f0o7s"
               "xtbymypperop310oddda58ildck20pq0e9e0fp@i.6.r6b.0.4b92bp7jgphh.vq1b"
               "anyisc6z7j@foo"
               "bcsiw04gzzjx38r@8kauzs.npurj6y"
               "audhssyp4xc8uy95i7qf96mni4bhoof2lll1wlti@lx5.xxcujr"
               "ui.ex.ay.cgc.btn35.v.r1p.r8.5gw2feti4.x.c.v.3lc.r.g.9.q.a.cq@3.r0.s.r6nrsnp49.w.60.5ux.qs62f"
               "o.w3.4j.5vn.w.w5.g.da.e3.qku.qn.g4x4m.hc4b.bq.c+qq9x8f0tapotv1@me.0y.q.k.q.lr.6r.m.65.5.2dly.21"
               "h.b7q6.n3ib+gb0ijd.uh7.rb.v.r.j55.z1.g0metssb.9q4.szrw.t.6.90.q.lok055p1sce.876wd@gba2cx7havgsd05"
               "qent76c1g4ts0n9pab7sopmov2wmze96p6aowg1xz@k.5q.d.o.v8.1.loq"
               "3.3.f.0.5@ci595duuz5h.p1z.f7x2v.ctomi75ggwho"
               "e2hiu@ktj6mf.o94m7pxw.n.5wut9vnb8cvn87h87f0o7s"
               "f9.fg.g4jcvc.1v1t.uy4kav.pmob@tim1xr1x.y.y5rq.i.v8za.m.orq69.gf.974s"
               "noi34@y3.8.z.p.m.s.9.g.w.6.5.f.5.l.4.t.8.v.q.i"
               "vvt6vprj7avn5v25ekl7z819vdhg0@fs.emlq.l.d5s.fxi.c.jdm1e.fq.5lgy.3.i.gs.6"
               "2+3.9.z.6.z.4@zofera.vd.1y.cav.1yp.j7.2"
               "c0rnm9u0q79bz0wv6fkub62@d4033jsnd.4pk2.97b4zpmcay3v4cxursto76bh60"
               "u.e.f9zq2yd.5of1q6.gu2.p.x.6d.o.4.k7.v.csdg6.g55+qnf.f.4i.e.89my.r.e9@a.an.7.u.4.6i.n.w.7.f.f.y.m.7"
               "ze7.6l.3.o.1.7.c.2.yo.4.2+wf.rk.i.5.93.z.50.8kkhr9x.ev@6lfsvcg975f.mnote6l57w2ab6vux4hc1q5dgcacf"
               "ze76l3o17c2yo42@6lfsvcg975f.mnote6l57w2ab6vux4hc1q5dgcacf"
               "52q6ozngbw89xugwx@au.n0jmq.5sptfw74.t.xmh3.7.gc"
               "oeillkhpneq@wdi.eed9akz.ud.r.vd1.ngyq9h.c.t.my.we5rx.0"
               "g46bpggtjnavz5mdsft3xwmiird5u2kyzp8bc126+vcv6kggs1ikj.kvb4m.w45np.i.98.g8.vq.qa.z6gus.n3i.s@tnr"
               "k.f.k.t.u.6.i.u.4.1wk.4.3.e.z.7.p.q.6.g.k.8.g.b@m.22.h.2.6.2cb5.l02.s.7q.a.b.1.f.gxp.04.n.p"
               "4487uqrrcm903t933i2ewcxjngohgvq3i66408ur05@lx5.xxcujr"
               "w2p0.1.u.t.p@7e.1.a"
               "t@jr0.kwaz.171.j717gahyh.ob.8ah.w.wnv4.z"
               "nsafp@8wbh9hflq4ry.bl0dfzovc5oh.nfhi7q98k.fb74d4ycr"
               "w.2.p.01.u.t.p+632677bp4qfvvn74a138g7rz2i8foauhpgeuf@7e.1.a"
               "d3.e.rf.3bavo.ka.x5.99.l.w7.e.eoi6.s.d.8ww6y.u.p.u.5c@nm.8z.84.htvf.sz.u1.s.2.t.7"
               "2+wk5mka49982o6fqx4bocl4qvvsbs1g8hux4egu789k3nxwb3hh43rp4w1tuq55n3bxpa@zofera.vd.1y.cav.1yp.j7.2"
               "exnkb2wtlzvx3isp8ddteovt4jz5k4q@c.6slt.qg3zv.ms.h.8d.w95jbz9.3.l.cwhr.x.a6.g8qowc"
               "loodrh2e911umho8f0wdixkl@a.xd1.1.w.r.v.s.5.2.t.5q.l.v.c.m.t.zs.6.o"
               "sas2gqgwt3f@o.rn.ynfc.v5dfannshda.b.d422z0uoz.djk4.vk.ff1kazbx"
               "uiexaycgcbtn35vr1pr85gw2feti4xcv3lcrg9qacq@lx5.xxcujr"
               "uef9zq2yd5of1q6gu2px6do4k7vcsdg6g55@a.an.7.u.4.6i.n.w.7.f.f.y.m.7"
               "oeillkhpneq+369peb.8ei.vy9opp72fe40.m2ra36sfyh.gzi9ppss@wdi.eed9akz.ud.r.vd1.ngyq9h.c.t.my.we5rx.0"
               "f9fgg4jcvc1v1tuy4kavpmob@tim1xr1x.y.y5rq.i.v8za.m.orq69.gf.974s"
               "dgiwmltske78oxuzgp03anr.ije8yaql.s3id+d.w.e@o.rn.ynfc.v5dfannshda.b.d422z0uoz.djk4.vk.ff1kazbx"
               "33f05@ci595duuz5h.p1z.f7x2v.ctomi75ggwho"
               "l.o.o.d.r.h.2.e.9.1.1.u.m.h.o8.f0.wd.ix.k.l+9.f.q.j.e.8.pb@a.xd1.1.w.r.v.s.5.2.t.5q.l.v.c.m.t.zs.6.o"
               "dgiwmltske78oxuzgp03anrije8yaqls3id@o.rn.ynfc.v5dfannshda.b.d422z0uoz.djk4.vk.ff1kazbx"
               "kfktu6iu41wk43ez7pq6gk8gb@m.22.h.2.6.2cb5.l02.s.7q.a.b.1.f.gxp.04.n.p"
               "f.9.f.g.g.4.j.c.v.c1.v.1.tu.y.4.k.a.v.pm.o.b@tim1xr1x.y.y5rq.i.v8za.m.orq69.gf.974s"
               "a.n.y.i.sc.6.z.7j@foo"
               "8btligw2n8v4sntw2dyhcmdk@fn.u.8"
               "g9@nm.8z.84.htvf.sz.u1.s.2.t.7"
               "s.as.2.gqg.wt3.f@o.rn.ynfc.v5dfannshda.b.d422z0uoz.djk4.vk.ff1kazbx"
               "v.v.t6.v.prj.7a.v.n.5.v.25.e.k.l.7.z.8.1.9v.d.h.g.0+0.7@fs.emlq.l.d5s.fxi.c.jdm1e.fq.5lgy.3.i.gs.6"
               "uiexaycgc.btn35vr1.pr85gw2feti4xcv3lcrg9qacq+fp22edazisk6quz4bxshh8r@3.r0.s.r6nrsnp49.w.60.5ux.qs62f"
               "g4.6.b.p.g.g.t.j.n.a.v.z5.mds.ft3.xw.m.iir.d.5u.2k.y.z.p.8b.c.1.2.6@tnr"
               "exnkb2wtlz.v.x3.isp.8.d.dte.ovt4jz5k4q+l73qxu8gd1v@c.6slt.qg3zv.ms.h.8d.w95jbz9.3.l.cwhr.x.a6.g8qowc"
               "uef.9.z.q.2.yd.5o.f.1q6g.u.2p.x6.do.4k.7.v.cs.d.g.6.g.55@a.an.7.u.4.6i.n.w.7.f.f.y.m.7"
               "hb7q6n3ib@gba2cx7havgsd05"
               "v9tqsa5qzf8y8c9698325b5g@i.2j.j.nm.m.4a.bi.r.y.t2.z8.66"
               "v.t.0.h.5+owa82t@jr0.kwaz.171.j717gahyh.ob.8ah.w.wnv4.z"
               "ndhz6b@h.5.y.d.t.p.r6.z.f.4p.t.5.9.n.4.0.9.29.m.o.j"
               "oeillkhpneq+a8az47u.l.dr9.8s4n.05o.bhw.8@wdi.eed9akz.ud.r.vd1.ngyq9h.c.t.my.we5rx.0"
               "3.k7gysj.2fz.m.s87.53.g.2614u0.j@j.y.w.tvbufj.1v8fs.5n9k6.3j5n.b9.5h.zwdwq24q3p8.vu"
               "m6uexeapg89ipqby2qvvc2gex195trc4j5iaaagu@hnqwhmv6i"
               "bcsiw04gzzjx38r@wdi.eed9akz.ud.r.vd1.ngyq9h.c.t.my.we5rx.0"
               "448.7.u.qrr.cm.90.3t.9.3.3i2e.w.cx.jn.g.o.hg.v.q.3i.66.4.0.8.ur0.5@lx5.xxcujr"]))))

;==========================================================
(deftest test-count-different-emails-exam-requirement
  (is (= 2 (count-different-emails
             ["ReCipiENt@eXaMPle.cOm"
              "recipient@example.com"
              "ReCipiENt@example.com"])))
  (is (= 1 (count-different-emails
             ["The.Same.Provider@Now.Here"
              "TheSameProvider+NothingMoreToSay@NOW.HERE"])))
  (is (= 1 (count-different-emails
             ["Barack.Hussein.Obama.II+44th.potus@ObamaPresidentialFoundation.org"
              "BarackHusseinObamaII@OBAMAPRESIDENTIALFOUNDATION.ORG"
              "B.a.r.a.c.k.H.u.s.s.e.i.n.O.b.a.m.a.I.I@obamapresidentialfoundation.org"])))
  (is (= 3 (count-different-emails
             ["Barack.Hussein.Obama.II+44th.potus@ObamaPresidentialFoundation.org"
              "BarackHusseinobamaII@OBAMAPRESIDENTIALFOUNDATION.ORG"
              "b.a.r.a.c.k.H.u.s.s.e.i.n.O.b.a.m.a.II@obamapresidentialfoundation.org"])))
  (is (= 6 (count-different-emails
             ["alice@e.mail"
              "eve@another.mail"
              "Bob+gOOd@e.Mail"
              "joe90@e.mail"
              "B.o.b@E.MAIL"
              "bob+new@e.Mail"
              "bob@another.provider"])))
  (is (= 64 (count-different-emails
              ["Bny5bjznzywy3l3rryn8idhwevunm5a8ojrbhPAD@303c.g.a"
               "o1.e.4.6.3.m.z1@p3.l.99.3x.lk.6z04.5o.fr.y.7"
               "6qqDEKEVOT4r9c9du@n9a.dn.sncl.m.5.dsb.0z.6TX.cl.s.0.O"
               "xrQK46@t5pc.DF1.c10i9o7.ufn.588JTWBZ0QZ.l1s"
               "5jyg1nrjr@k5igfp.rmvot.m.9m.ut2.qcpt73.9y.d3qgrl44.w.3g6.0jt"
               "e5s4tlj7m9nmp2zbcokn@k.PAS9.x.e.osn00gw.tf.4n.3fftne.4o5.njapw.ml.a"
               "g9ycpm@L7"
               "nfce84lmp59k4c@2.6oc.2f5w.f8.vip.96.d7.bkoh4.6uikn.r.j.d6e.UN"
               "a5cceqzvib6l7w6y7wdlkw5pf2jj03z3ojtmr4v3w5@j5m.yoj.u7"
               "6.6hbz+rmypyzgevtn63jvbzz4km161zsj26y42g7tjfop1gn5s4vjgug@ja.c.m"
               "tz8oswiuvfp9hy3ebk1w4651n1z8d0xv@R.H223D91C0FSF1VTZB828KA75"
               "bj.kjd.uo@svzx46upvk2fy.i.bn.xjmfzb4094yomfer771q"
               "jr763ef1bn7vu78iraupnfsvgk2p827ad@e.4po.cq.w.9"
               "e5.s.4t.l.j.7.m.9n.m.p2.z.b.c.o.kn+b.f.yf@K.PAS9.X.E.OSN00GW.TF.4N.3FFTNE.4O5.NJAPW.ML.A"
               "a5cceq.zvib6l7w6y7w.dlk.w5pf2jj03.z3.ojt.m.r.4.v.3w.5@exgv4.zfih.b.p.0uko5e.g.jrh.4.bh.l6.ceh.0u"
               "uj03jia2@c.s.2.n.3.z.o.6w.a.tbyk.ka.w.j.x.4q0.l.ns.2.f.e"
               "u.j.0.3.j.i.a.2@c.s.2.n.3.z.o.6w.a.tbyk.ka.w.j.x.4q0.l.ns.2.f.e"
               "u30e97blh9uchuxk3e215efyidh6uukrqcw470mg@jtwrfl.5.10.4.ov.baflwl.ol8de.7d73.7b8o.h"
               "iy9@r0u77g1jp.u4"
               "h.uf.v.0.f.3.g.z.j31.l.PGE.Vp.z.z.g@B45SZ.r2d.vj3q.o.0q9c1r9yxa.u.7.c7oncxh.jyeh"
               "xt9tM7y1eu4b2zbqvm71gj0yikasyxnn@X.8.2.f"
               "x.r.Q.K.4.6@t5pc.df1.c10i9o7.UFN.588jtwbz0qz.L1S"
               "b.j.k.j.d.u.o@Svzx46upvk2fy.I.Bn.Xjmfzb4094yomfer771q"
               "izu2jfp0uzbg24jx67y77voc9zwkuc1ch@udqui2dos4.maapcz4155jseo3pfrbspz7r.tyah"
               "omdcvkcd9edqep2xi3ghot5hbyclhg@s.5.um5n.iu62wlHEL2JP.nxm8aw8.2z7s.ts"
               "KLOYJ820@r.n.d.y.z.w.1.vr.8"
               "iwqcqgax9je026r@i2.qh.6.mv.r.c.g.8.9m.sj.t.rg.l.z"
               "6.6h.b.z+rx2j.9.4c.b5j610.a.7z3t.c.b.nms5.rgfbhbo4.48d26sy5x.h.q.zflm.j@ja.c.m"
               "f8vhydl@nojbgw3f0d.gutnr25oneye.dk.i26.uo"
               "66hbz@qxgurf.cw3moa1877na008get8awkBBBkf1j5hv282p"
               "jr763ef1bn7vu78iraupnfsvgk2p827ad@B4.8.I.P.I.R.5.I.2G.R.LT.V"
               "hufv0f3gzj31lPGEVpzzg@b45sz.r2d.vj3q.o.0q9c1r9yxa.u.7.c7oncxh.jyeh"
               "66hbz@ja.c.m"
               "jr.763ef1bn7vu78iraup.nfsvgk2p8.27a.d+1aa6u.XYKU3G63lky1tzy2q71@B4.8.i.p.i.r.5.i.2g.r.lt.v"
               "82a8vb31yfpuctt4sq6ij@44.n7ufr2on.3t.gzc"
               "Bn.y5bj.znz.yw.y3l.3rryn8i.dhwevunm5.a8o.jr.bhPAD@303C.G.A"
               "6nqz972859jaa4kfnmfw6u0j@745xex.2gkqy.3ufzlb3ujy.t79.lft1.t.e"
               "Jr763ef1bn7vu78iraupnfsvgk2p827AD@ji.e.c.f.3.66.s.z.0"
               "x.t.9t.M7.y1.e.u.4.b.2.zbq.v.m.7.1.gj.0.y.i.k.a.s.y.x.n.n+4ielf10ih1b3s3f.fe59d.hz.yw5n05f@x.8.2.F"
               "FCV9@c.c.nx.x.4.y.xn.x.s.y.q.v.v.qe.h.i.g"
               "u0epd7ctmwxojzen@udqui2dos4.MAAPCZ4155jseo3pfrbspz7r.tyah"
               "vq4@ghobxucuo1zkxtjdhhsw0h"
               "a5c.c.eqzvib6l7w6y7wdlkw5pf2j.j0.3.z3ojtmr4v3w5+9.gie@EXGV4.ZFIH.b.p.0uko5e.g.jrh.4.bh.l6.ceh.0u"
               "n.fc.e.8.4.l.m.p.5.9.k.4.c+iyv.041299@2.6oc.2f5w.f8.vip.96.d7.bkoh4.6uikn.r.j.d6e.UN"
               "f.c.v.9+q.t.c.x.s.b.j.m.0.v.e.p.kw.9.b.ic.2.m.r.u3.3.1@C.C.NX.X.4.Y.XN.X.S.Y.Q.V.V.QE.H.I.G"
               "79vgun6qsu63r64hdupo7ibh4lx9xs2@l.t"
               "4kn7rynfity1cgxnbdh3btg5mk3coew7qhre2equa@5.4ap3.0xyjzpdv9x6qc14kne.l.wepyj.rh3w7ws"
               "nblyp4dxkuao2nzpfcb6p9efu8r5r@5.t.pt.8.g.9.ma.i.1w.v.j.q.jm.b.h.w.pm.75.a.f.e5"
               "6xgxaprc2s653cxf5hmke3fjn8gorf6o9uvmtkq@ru.r.bxrkkzxnabukuo"
               "u.30.e97blh9u.chuxk3e.2.15.efyidh6.u.u.kr.q.cw470mg+qwt.2@jtwrfl.5.10.4.ov.baflwl.ol8de.7d73.7b8o.h"
               "6.6.hbz+k.3.x.6@qxgurf.cw3moa1877na008get8awkbbbkf1j5hv282p"
               "bjkjduo@svzx46upvk2fy.i.bn.xjmfzb4094yomfer771q"
               "l.i.J.nx.3.do.t.z.vr.qu.5.X.Z@qexq0x6355ojprkokvuzws8l.qnn5jqym5mw6jn"
               "a5cceqzvib6l7w6y7wdlkw5pf2jj03z3ojtmr4v3w5@exgv4.zfih.b.p.0uko5e.g.jrh.4.bh.l6.ceh.0u"
               "KLOY.J.8.20+9.mh7c83wf.3.qw2fsjty3wt0.5cx7pvrj.v2vpu.hb.bs.tfn4.dgqln51l6.7.1qd@r.n.d.y.z.w.1.vr.8"
               "omdcvk.cd.9edqep2xi3ghot5hbyclhg+z.y5d.3.f.kb.x.1.0vs.da.3h.m@s.5.um5n.iu62wlhel2jp.nxm8aw8.2z7s.ts"
               "liJnx3dotzvrqu5XZ@QEXQ0X6355OJPRKOKVUZWS8L.qnn5Jqym5mw6Jn"
               "oflu8s4@8.eo.34ocu88d7.je7q"
               "5oee2ls3p4qgivj0qosnxbze2a7ov2gt77dv1ay5mz@j5m.yoj.u7"
               "6.qqDE.KE.V.O.T4r.9.c.9d.u@N9a.dn.sncl.m.5.dsb.0z.6tX.cl.S.0.o"
               "gc2a6p39u8mnya7k0o816y366GJYUCAHOUP@5.4ap3.0xyjzpdv9x6qc14kne.l.wepyj.rh3w7ws"
               "kj8oj6kvj3nzcrm4h2arnxtcfbroejzst9kjn@v.m.h.k.wd.9w.qm.kd.o.t.o.f.7.mp.kf.9.i.gw.f.we"
               "juf7hmv8ou2j45ln55e@NGR.H.R.4.E.3.P6H.NTS.G6J.9M.0.8.P.E.N"
               "66hbz@d4imief4.u.0ik.v.b74"
               "x.t.9.t.M.7.y.1.e.u.4.b.2.z.b.q.v.m.7.1.g.j.0.y.i.k.a.s.y.x.n.n@x.8.2.f"
               "omdcvk.cd9e.dq.ep2.xi3.gh.o.t5h.byclhg+nky979txecf867ll4blso2s@s.5.um5n.iu62wlhel2jp.nxm8aw8.2z7s.ts"
               "jr763ef1bn7vu78iRAUPnfsvgk2p827ad@j.6.l.w.y.z.iou"
               "wzz45FIRD1zhpusr@BC06EQ3MN.lph.1qhwtj2e3naa7f9b4aebmk2z2xcy"
               "iwqcq.gax9je0.26r@I2.QH.6.MV.R.C.G.8.9M.SJ.T.RG.L.Z"
               "515.zfgoc1a0e6k22n.bgsxrxv43ckmoxqp6jz7p+o.b.m.f.p.9.1.b.d.p@mlzr.er.wqh.xt.cj.s.1bq.m.bw5.kgf6"
               "6.6h.bz@d4imief4.u.0ik.v.b74"
               "Jr.76.3ef1bn.7.vu.7.8.ira.u.pn.f.s.v.gk.2.p82.7A.D+xu2kqtje2.cpsaazhsqt8blpn732p@ji.e.c.f.3.66.s.z.0"
               "kj8oj6kvj3nzcrm4h2arnxtcfbroejzst9kjn@m.4r.ij.w.i.t.u.c9.en6.ul.2d.o.zmvpa.t.f.5a4.k.rz"
               "jofydjcv7fbr04xby87gds53bgg868zh@d4imief4.u.0ik.v.b74"
               "o1e463mz1+w4.g.n.p.pq2.l.5.g.h.9.5.64s.911.s.9.h.uor.vhc.3.x.z.2e@p3.l.99.3x.lk.6z04.5o.fr.y.7"
               "a.5cceqz.vib6l7.w6y7wd.lkw5pf2jj03z3.oj.tmr4.v3w5@exgv4.zfih.b.p.0uko5e.g.jrh.4.bh.l6.ceh.0u"
               "j.r7.6.3.ef.1.b.n.7v.u.78i.r.a.u.p.n.fs.v.gk.2p.82.7.a.d@b4.8.i.p.i.r.5.i.2g.r.lt.v"
               "FC.V9@c.c.nx.x.4.Y.XN.X.S.Y.Q.V.V.QE.H.I.G"
               "gno1el4z@k.iw"
               "515zfgoc1a0e6.k2.2n.bgsxrx.v.43c.km.o.x.qp.6.jz7.p@mlzr.er.wqh.xt.cj.s.1bq.m.bw5.kgf6"
               "7jab22v614iyxc6ey9mqydm31f3ug49bso@ac4qog9rnht.0k.pr91w3p.t.6.4oft.wq8e5nk.v5r.c"
               "wzz.45fird1zhpus.r@bc06eq3mn.LPH.1QHWTJ2E3NAA7F9B4AEBMK2Z2XCY"
               "wo6feuu@t.b.m.b.3.77.gx.5.c.kh.j.w.y.f.7x3.t.b.c.r"
               "o.m.d.c.vkcd.9.e.dq.e.p.2.x.i3.gh.o.t.5.hb.y.c.l.h.g+c@s.5.um5n.iu62wlhel2jp.nxm8aw8.2z7s.ts"
               "6n.q.z.9.7.2.8.5.9.j.a.a.4.k.f.nm.f.w.6u.0.j+r.7.y.7.z.e.k.c.p@745xex.2gkqy.3ufzlb3ujy.t79.lft1.t.e"
               "gc2a6p39u8mnya7k0o.81.6y366GJYUCAHOUP+vb530nnz.c.fqonm7g@5.4ap3.0xyjzpdv9x6qc14kne.l.wepyj.rh3w7ws"
               "uj03jia2@vnca2.a7.18.vj"
               "515zfgoc1a0e6k22nbgsxrxv43ckmoxqp6jz7p@mlzr.er.wqh.xt.cj.s.1bq.m.bw5.kgf6"
               "o1e463mz1@p3.l.99.3x.lk.6z04.5o.fr.y.7"
               "H30g0jq@n9a.dn.SNCL.M.5.DSB.0Z.6tx.cl.s.0.O"
               "1crfmp59tf2160na0ss6empxk8@a"
               "c6gexqyxxhyil3yftlaho9n5hh7ieawfpmfvhb2@45.0ug7odyj.zmrq.mpt.4"
               "gk4w9qf21z3ynfb5im7jp7t786y52ubktmyo1@d.4.5n.ukaixj.qa.7.qh.lu.38eq.ed.6.yk"
               "97m2pyin0m1vifoi7jt15a7lv@j6.w.zwx.0"
               "fcv9@4as0.t.DQKZZABSBHV26P08I.2043.3p"
               "vq4@4.u5.b.b.ywdo8.siv5.uc.9o.h9axqaiqrx8.v.afqbyo.n"
               "t97zo41qxr38v905x@ws"
               "H.3.0.g.0.j.q@N9A.DN.sncl.m.5.dsb.0z.6tx.cl.s.0.o"
               "tz8oswiuvfp9hy3ebk1w4651n1z8d0xv@7.o.T.G.9.8.r.z.2.8.ax.k.6.s.jb.f.s.6"
               "515zfgoc1a0e6k22nbgsxrxv43ckmoxqp6jz7p@J.6.L.W.Y.Z.IOU"]))))

;==========================================================
(run-tests)
