# 📊 Análise de Desempenho Estudantil

Projeto de Análise Exploratória de Dados (EDA) e Regressão Linear em R, com dashboard interativo em Shiny, para identificar os principais fatores que influenciam o desempenho de estudantes em exames finais.

## 🎯 Objetivo

Entender quais variáveis (horas de estudo, frequência, sono, contexto familiar, etc.) mais influenciam a nota final dos alunos, através de:
1. Análise exploratória de dados (EDA)
2. Modelo de regressão linear
3. Dashboard interativo para exploração dos dados e simulação de previsões

## 🗂️ Dataset

| Coluna | Descrição |
|---|---|
| `student_id` | Identificador do aluno |
| `gender` | Gênero |
| `study_time_hours` | Horas de estudo por semana |
| `attendance_percent` | Percentual de frequência |
| `sleep_hours` | Horas de sono por noite |
| `parental_education` | Escolaridade dos pais |
| `internet_access` | Acesso à internet (Sim/Não) |
| `extracurricular_activities` | Participa de atividades extracurriculares |
| `part_time_job` | Trabalho de meio período |
| `previous_grade` | Nota da avaliação anterior |
| `final_exam_score` | Nota final (variável alvo) |
| `final_grade` | Conceito final (derivado da nota) |

## 🚀 Como rodar

### Notebook (EDA + Modelo)
1. Abra `analise_desempenho_estudantil.ipynb` no [Google Colab](https://colab.research.google.com)
2. Troque o runtime para **R**: `Ambiente de execução → Alterar tipo de ambiente de execução → R`
3. Execute as células em ordem

### Dashboard (Shiny)
Acesse em: [https://estevaobm.shinyapps.io/dashboard_analise_estudantil/](ttps://estevaobm.shinyapps.io/dashboard_analise_estudantil/)

## 🛠️ Tecnologias

- **R** + `tidyverse`: manipulação e visualização de dados
- **ggplot2 / plotly / GGally / corrplot**: EDA visual e interativa
- **broom / performance**: modelagem e diagnóstico estatístico
- **shiny / shinydashboard**: dashboard interativo

## 📈 Principais achados

- `study_time_hours`, `attendance_percent` e `previous_grade` são os fatores com maior poder explicativo sobre a nota final.
- Variáveis como gênero e trabalho de meio período têm efeito menor ou não significativo.
- Detalhes completos, limitações do modelo e discussão estatística estão no notebook.
