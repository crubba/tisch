tex_templates <- "
\\begin{table}{{table_position}}\\begin{threeparttable}\\{{text_size}}
\t{{caption}}{{label}}
\\centering

\\begin{tabular}{ {{dim_str}} }
\t \\toprule
\t {{header_str}}
\t \\midrule
\t {{body_str}} \\\\
\t \\bottomrule
\t \\end{tabular}

\t {{footnote}}
\\end{threeparttable}
\\end{table}"