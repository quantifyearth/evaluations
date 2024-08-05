# evaluations

R notebooks for running ex-post evaluations based on the outputs of the `tmf-implementation`.

To knit the notebook, run the following line in a shell terminal, replacing the `params` as appropriate:

`Rscript -e "rmarkdown::render(input='~/evaluations/R/ex_post_evaluation_template.Rmd', output_file='~/evaluations/R/example_project.html', params=list(proj='example_project', t0=2010, eval_year=2022, input_dir='~/projects', output_dir='~/tmf_pipe_out', evaluations_dir='~/evaluations'))"`

For detailed instructions please see the 4C [Notion page](https://www.notion.so/forests/Running-evaluations-on-sherwood-c3073b10e62c4169b1769b0f4ca81300).
