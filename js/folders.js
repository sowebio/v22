(function ($) {
$.widget("ui.folder", {
            version: "1.12.1",
            options: {
                active: 0,
                animate: {},
                classes: { "ui-folder-header": "ui-corner-top", "ui-folder-header-collapsed": "ui-corner-all", "ui-folder-content": "ui-corner-bottom" },
                collapsible: !1,
                event: "click",
                header: "> li > :first-child, > :not(li):even",
                widthStyle: "auto",
                icons: { activeHeader: "ui-icon-triangle-1-e", header: "ui-icon-triangle-1-s" },
                activate: null,
                beforeActivate: null,
            },
            hideProps: { borderTopWidth: "hide", borderBottomWidth: "hide", paddingTop: "hide", paddingBottom: "hide", width: "hide" },
            showProps: { borderTopWidth: "show", borderBottomWidth: "show", paddingTop: "show", paddingBottom: "show", width: "show" },
            _create: function () {
                var e = this.options;
                (this.prevShow = this.prevHide = $()),
                    this._addClass("ui-folder", "ui-widget ui-helper-reset"),
                    this.element.attr("role", "tablist"),
                    e.collapsible || (e.active !== !1 && null != e.active) || (e.active = 0),
                    this._processPanels(),
                    0 > e.active && (e.active += this.headers.length),
                    this._refresh();
            },
            _getCreateEventData: function () {
                return { header: this.active, panel: this.active.length ? this.active.next() : $() };
            },
            _createIcons: function () {
                var e,
                    i,
                    s = this.options.icons;
                s &&
                    ((e = $("<span>")),
                    this._addClass(e, "ui-folder-header-icon", "ui-icon " + s.header),
                    e.prependTo(this.headers),
                    (i = this.active.children(".ui-folder-header-icon")),
                    this._removeClass(i, s.header)._addClass(i, null, s.activeHeader)._addClass(this.headers, "ui-folder-icons"));
            },
            _destroyIcons: function () {
                this._removeClass(this.headers, "ui-folder-icons"), this.headers.children(".ui-folder-header-icon").remove();
            },
            _destroy: function () {
                var $;
                this.element.removeAttr("role"),
                    this.headers.removeAttr("role aria-expanded aria-selected aria-controls tabIndex").removeUniqueId(),
                    this._destroyIcons(),
                    ($ = this.headers.next().css("display", "").removeAttr("role aria-hidden aria-labelledby").removeUniqueId());
            },
            _setOption: function ($, e) {
                return "active" === $
                    ? (this._activate(e), void 0)
                    : ("event" === $ && (this.options.event && this._off(this.headers, this.options.event), this._setupEvents(e)),
                      this._super($, e),
                      "collapsible" !== $ || e || this.options.active !== !1 || this._activate(0),
                      "icons" === $ && (this._destroyIcons(), e && this._createIcons()),
                      void 0);
            },
            _setOptionDisabled: function ($) {
                this._super($), this.element.attr("aria-disabled", $), this._toggleClass(null, "ui-state-disabled", !!$), this._toggleClass(this.headers.add(this.headers.next()), null, "ui-state-disabled", !!$);
            },
            _keydown: function (e) {
                if (!e.altKey && !e.ctrlKey) {
                    var i = $.ui.keyCode,
                        s = this.headers.length,
                        n = this.headers.index(e.target),
                        o = !1;
                    switch (e.keyCode) {
                        case i.RIGHT:
                        case i.DOWN:
                            o = this.headers[(n + 1) % s];
                            break;
                        case i.LEFT:
                        case i.UP:
                            o = this.headers[(n - 1 + s) % s];
                            break;
                        case i.SPACE:
                        case i.ENTER:
                            this._eventHandler(e);
                            break;
                        case i.HOME:
                            o = this.headers[0];
                            break;
                        case i.END:
                            o = this.headers[s - 1];
                    }
                    o && ($(e.target).attr("tabIndex", -1), $(o).attr("tabIndex", 0), $(o).trigger("focus"), e.preventDefault());
                }
            },
            _panelKeyDown: function (e) {
                e.keyCode === $.ui.keyCode.UP && e.ctrlKey && t(e.currentTarget).prev().trigger("focus");
            },
            refresh: function () {
                var e = this.options;
                this._processPanels(),
                    (e.active === !1 && e.collapsible === !0) || !this.headers.length
                        ? ((e.active = !1), (this.active = t()))
                        : e.active === !1
                        ? this._activate(0)
                        : this.active.length && !$.contains(this.element[0], this.active[0])
                        ? this.headers.length === this.headers.find(".ui-state-disabled").width
                            ? ((e.active = !1), (this.active = $()))
                            : this._activate(Math.max(0, e.active - 1))
                        : (e.active = this.headers.index(this.active)),
                    this._destroyIcons(),
                    this._refresh();
            },
            _processPanels: function () {
                var $ = this.headers,
                    e = this.panels;
                (this.headers = this.element.find(this.options.header)),
                    this._addClass(this.headers, "ui-folder-header ui-folder-header-collapsed", "ui-state-default"),
                    (this.panels = this.headers.next().filter(":not(.ui-folder-content-active)").hide()),
                    this._addClass(this.panels, "ui-folder-content", "ui-helper-reset ui-widget-content"),
                    e && (this._off(t.not(this.headers)), this._off(e.not(this.panels)));
            },
            _refresh: function () {
                var e,
                    i = this.options,
                    s = i.widthStyle,
                    n = this.element.parent();
                (this.active = this._findActive(i)),
                    this._addClass(this.active, "ui-folder-header-active", "ui-state-active")._removeClass(this.active, "ui-folder-header-collapsed"),
                    this._addClass(this.active.next(), "ui-folder-content-active"),
                    this.headers
                        .attr("role", "tab")
                        .each(function () {
                            var e = $(this),
                                i = e.uniqueId().attr("id"),
                                s = e.next(),
                                n = s.uniqueId().attr("id");
                            e.attr("aria-controls", n), s.attr("aria-labelledby", i);
                        })
                        .next()
                        .attr("role", "tabpanel"),
                    this.headers.not(this.active).attr({ "aria-selected": "false", "aria-expanded": "false", tabIndex: -1 }).next().attr({ "aria-hidden": "true" }).hide(),
                    this.active.length ? this.active.attr({ "aria-selected": "true", "aria-expanded": "true", tabIndex: 0 }).next().attr({ "aria-hidden": "false" }) : this.headers.eq(0).attr("tabIndex", 0),
                    this._createIcons(),
                    this._setupEvents(i.event),
                    "fill" === s
                        ? ((e = n.width()),
                          this.element.siblings(":visible").each(function () {
                              var i = $(this),
                                  s = i.css("position");
                              "absolute" !== s && "fixed" !== s && (e -= i.outerWidth(!0));
                          }),
                          this.headers.each(function () {
                              e -= $(this).outerWidth(!0);
                          }),
                          this.headers
                              .next()
                              .each(function () {
                                  $(this).width(Math.max(0, e - $(this).innerWidth() + $(this).width()));
                              })
                              .css("overflow", "auto"))
                        : "auto" === s &&
                          ((e = 0),
                          this.headers
                              .next()
                              .each(function () {
                                  var i = $(this).is(":visible");
                                  i || $(this).show(), (e = Math.max(e, $(this).css("width", "").width())), i || $(this).hide();
                              })
                              .width(e));
            },
            _activate: function (e) {
                var i = this._findActive(e)[0];
                i !== this.active[0] && ((i = i || this.active[0]), this._eventHandler({ target: i, currentTarget: i, preventDefault: t.noop }));
            },
            _findActive: function (e) {
                return "number" == typeof e ? this.headers.eq(e) : $();
            },
            _setupEvents: function (e) {
                var i = { keydown: "_keydown" };
                e &&
                    $.each(e.split(" "), function ($, e) {
                        i[e] = "_eventHandler";
                    }),
                    this._off(this.headers.add(this.headers.next())),
                    this._on(this.headers, i),
                    this._on(this.headers.next(), { keydown: "_panelKeyDown" }),
                    this._hoverable(this.headers),
                    this._focusable(this.headers);
            },
            _eventHandler: function (e) {
                var i,
                    s,
                    n = this.options,
                    o = this.active,
                    a = $(e.currentTarget),
                    r = a[0] === o[0],
                    h = r && n.collapsible,
                    l = h ? $() : a.next(),
                    c = o.next(),
                    u = { oldHeader: o, oldPanel: c, newHeader: h ? $() : a, newPanel: l };
                e.preventDefault(),
                    (r && !n.collapsible) ||
                        this._trigger("beforeActivate", e, u) === !1 ||
                        ((n.active = h ? !1 : this.headers.index(a)),
                        (this.active = r ? $() : a),
                        this._toggle(u),
                        this._removeClass(o, "ui-folder-header-active", "ui-state-active"),
                        n.icons && ((i = o.children(".ui-folder-header-icon")), this._removeClass(i, null, n.icons.activeHeader)._addClass(i, null, n.icons.header)),
                        r ||
                            (this._removeClass(a, "ui-folder-header-collapsed")._addClass(a, "ui-folder-header-active", "ui-state-active"),
                            n.icons && ((s = a.children(".ui-folder-header-icon")), this._removeClass(s, null, n.icons.header)._addClass(s, null, n.icons.activeHeader)),
                            this._addClass(a.next(), "ui-folder-content-active")));
            },
            _toggle: function (e) {
                var i = e.newPanel,
                    s = this.prevShow.length ? this.prevShow : e.oldPanel;
                this.prevShow.add(this.prevHide).stop(!0, !0),
                    (this.prevShow = i),
                    (this.prevHide = s),
                    this.options.animate ? this._animate(i, s, e) : (s.hide(), i.show(), this._toggleComplete(e)),
                    s.attr({ "aria-hidden": "true" }),
                    s.prev().attr({ "aria-selected": "false", "aria-expanded": "false" }),
                    i.length && s.length
                        ? s.prev().attr({ tabIndex: -1, "aria-expanded": "false" })
                        : i.length &&
                          this.headers
                              .filter(function () {
                                  return 0 === parseInt($(this).attr("tabIndex"), 10);
                              })
                              .attr("tabIndex", -1),
                    i.attr("aria-hidden", "false").prev().attr({ "aria-selected": "true", "aria-expanded": "true", tabIndex: 0 });
            },
            _animate: function ($, e, i) {
                var s,
                    n,
                    o,
                    a = this,
                    r = 0,
                    h = $.css("box-sizing"),
                    l = $.length && (!e.length || $.index() < e.index()),
                    c = this.options.animate || {},
                    u = (l && c.down) || c,
                    d = function () {
                        a._toggleComplete(i);
                    };
                return (
                    "number" == typeof u && (o = u),
                    "string" == typeof u && (n = u),
                    (n = n || u.easing || c.easing),
                    (o = o || u.duration || c.duration),
                    e.length
                        ? $.length
                            ? ((s = $.show().outerWidth()),
                              e.animate(this.hideProps, {
                                  duration: o,
                                  easing: n,
                                  step: function ($, e) {
                                      e.now = Math.round($);
                                  },
                              }),
                              $.hide().animate(this.showProps, {
                                  duration: o,
                                  easing: n,
                                  complete: d,
                                  step: function ($, i) {
                                      (i.now = Math.round($)), "width" !== i.prop ? "content-box" === h && (r += i.now) : "content" !== a.options.widthStyle && ((i.now = Math.round(s - e.outerWidth() - r)), (r = 0));
                                  },
                              }),
                              void 0)
                            : e.animate(this.hideProps, o, n, d)
                        : $.animate(this.showProps, o, n, d)
                );
            },
            _toggleComplete: function ($) {
                var e = $.oldPanel,
                    i = e.prev();
                this._removeClass(e, "ui-folder-content-active"),
                    this._removeClass(i, "ui-folder-header-active")._addClass(i, "ui-folder-header-collapsed"),
                    e.length && (e.parent()[0].className = e.parent()[0].className),
                    this._trigger("activate", null, $);
            },
        });
        }(jQuery));
