(load "../data/gesamt.lsp")
(load "libs/vendor/versionspace.lsp")
(load "libs/create_neg_pos.lsp")
(load "libs/aq.lsp")
 
(print "Start Training")
(setq K (do-AQ pexamples nexamples))
(print "End Training")
(print "K:")
(print K)