# Codebook do banco de estabelecimentos religiosos utilizado na classificação semi-supervisionada
## Unidade de análise: estabelecimento religioso

As seguintes variáveis podem ser acessadas no arquivo "df_igrejas_nomes.csv":

$ cnpj                     <dbl> informa o CNPJ do estabelecimento religioso 
$ codigo_natureza_juridica <int> informa o código de natureza jurídica do estabelecimento religioso
$ razao_social             <chr> informa a razão social (nome) do estabelecimento religioso; esta é a variável texto utilizada para a classificação dos estabelecimentos religiosos inscritos na Receita Federal
$ uf                       <chr> informa a Unidade Federativa (UF) onde está localizado o estabelecimento religioso
$ municipio                <chr> informa o município brasileiro onde está localizado o estabelecimento religioso
$ cep                      <dbl> informa o códio postal do estabelecimento religioso
$ bairro                   <chr> informa o bairro do estabelecimento religioso
$ logradouro               <chr> informa o logradouro do estabelecimento religioso
$ numero                   <chr> informa o número de logradouro do estabelecimento religioso
$ ano_inicio               <int> informa o ano de ativação (criação do estabelecimento religioso)
$ idade_em_19              <int> informa a idade do estabelecimento religioso na data da coleta da informação (2019)
$ count_XXXX               <int> informa se um dado estabelecimento religioso existia naquele ano. Por exemplo, count_1966 informa se um dado estabelecimento religioso estava inscrito (tinha um CNPJ) na Receita Federal no ano de 1996
  
