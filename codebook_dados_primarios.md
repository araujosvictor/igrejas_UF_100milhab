# Codebook do banco de estabelecimentos religiosos utilizado na classificação semi-supervisionada
## Unidade de análise: estabelecimento religioso

O arquivo "df_igrejas_nomes.csv" contém as seguintes variáveis:

1. $ cnpj                     <dbl> informa o CNPJ do estabelecimento religioso 
2. $ codigo_natureza_juridica <int> informa o código de natureza jurídica do estabelecimento religioso
3. $ razao_social             <chr> informa a razão social (nome) do estabelecimento religioso; esta é a variável texto utilizada para a classificação dos estabelecimentos religiosos inscritos na Receita Federal
4. $ uf                       <chr> informa a Unidade Federativa (UF) onde está localizado o estabelecimento religioso
5. $ municipio                <chr> informa o município brasileiro onde está localizado o estabelecimento religioso
6. $ cep                      <dbl> informa o códio postal do estabelecimento religioso
7. $ bairro                   <chr> informa o bairro do estabelecimento religioso
8. $ logradouro               <chr> informa o logradouro do estabelecimento religioso
9. $ numero                   <chr> informa o número de logradouro do estabelecimento religioso
10. $ ano_inicio               <int> informa o ano de ativação (criação do estabelecimento religioso)
11. $ idade_em_19              <int> informa a idade do estabelecimento religioso na data da coleta da informação (2019)
12. $ count_XXXX               <int> informa se um dado estabelecimento religioso existia naquele ano. Por exemplo, count_1966 informa se um dado estabelecimento religioso estava inscrito (tinha um CNPJ) na Receita Federal no ano de 1996
  
