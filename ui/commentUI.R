###data cleaning UI
ui.comment <- function(){
  tagList(
    column(width = 8, offset = 2,
           h4("If you have any questions about MetFlow,
              please do not hesitate to leave us a message!",
              br(),
              "You can also send email to us, zhulab@sioc.ac.cn.",
              align = "justify",
              style="line-height:40px;color:black"),
           tabPanel(title = "Comments", icon = icon(name = "comments"),
                    tabPanel(title = "disqus_here",
                             div(id="disqus_thread",
                                 HTML(
                                   "<script>(function() {
                                   var d = document, s = d.createElement('script');

                                   s.src = '//omictools.disqus.com/embed.js';

                                   s.setAttribute('data-timestamp', +new Date());
                                   (d.head || d.body).appendChild(s);
})();
                                   </script>
                                   <noscript>Please enable JavaScript to view the <a href='https://disqus.com/?ref_noscript' rel='nofollow'>comments powered by Disqus.</a></noscript>"
                             )
                             )
                             )
                             ),
           div(HTML('<script id="dsq-count-scr" src="//omictools.disqus.com/count.js" async></script>'))
                             ),
    br(), br(), br()
    # )
                    )}