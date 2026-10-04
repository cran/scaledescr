#' Counterbalanced PHQ-9 Example Data
#'
#' A small dummy dataset simulating a counterbalanced administration of the
#' PHQ-9 in English and Gujarati, as exported from platforms such as
#' PsyToolkit. Each participant completed the English and Gujarati versions
#' in one of two orders, so the unused branch for each language is \code{NA}.
#'
#' @format A data frame with 8 rows and 39 variables:
#' \describe{
#'   \item{psy_group}{Counterbalancing group: 1 = English first, then Gujarati; 2 = Gujarati first, then English.}
#'   \item{age}{Age in years.}
#'   \item{gender}{Gender: 1 = male, 2 = female.}
#'   \item{phq_e_1_1}{English PHQ-9 item 1, Order 1 (group 1), scored 0 to 3.}
#'   \item{phq_e_1_2}{English PHQ-9 item 2, Order 1 (group 1), scored 0 to 3.}
#'   \item{phq_e_1_3}{English PHQ-9 item 3, Order 1 (group 1), scored 0 to 3.}
#'   \item{phq_e_1_4}{English PHQ-9 item 4, Order 1 (group 1), scored 0 to 3.}
#'   \item{phq_e_1_5}{English PHQ-9 item 5, Order 1 (group 1), scored 0 to 3.}
#'   \item{phq_e_1_6}{English PHQ-9 item 6, Order 1 (group 1), scored 0 to 3.}
#'   \item{phq_e_1_7}{English PHQ-9 item 7, Order 1 (group 1), scored 0 to 3.}
#'   \item{phq_e_1_8}{English PHQ-9 item 8, Order 1 (group 1), scored 0 to 3.}
#'   \item{phq_e_1_9}{English PHQ-9 item 9, Order 1 (group 1), scored 0 to 3.}
#'   \item{phq_g_1_1}{Gujarati PHQ-9 item 1, Order 1 (group 2), scored 0 to 3.}
#'   \item{phq_g_1_2}{Gujarati PHQ-9 item 2, Order 1 (group 2), scored 0 to 3.}
#'   \item{phq_g_1_3}{Gujarati PHQ-9 item 3, Order 1 (group 2), scored 0 to 3.}
#'   \item{phq_g_1_4}{Gujarati PHQ-9 item 4, Order 1 (group 2), scored 0 to 3.}
#'   \item{phq_g_1_5}{Gujarati PHQ-9 item 5, Order 1 (group 2), scored 0 to 3.}
#'   \item{phq_g_1_6}{Gujarati PHQ-9 item 6, Order 1 (group 2), scored 0 to 3.}
#'   \item{phq_g_1_7}{Gujarati PHQ-9 item 7, Order 1 (group 2), scored 0 to 3.}
#'   \item{phq_g_1_8}{Gujarati PHQ-9 item 8, Order 1 (group 2), scored 0 to 3.}
#'   \item{phq_g_1_9}{Gujarati PHQ-9 item 9, Order 1 (group 2), scored 0 to 3.}
#'   \item{phq_e_2_1}{English PHQ-9 item 1, Order 2 (group 2), scored 0 to 3.}
#'   \item{phq_e_2_2}{English PHQ-9 item 2, Order 2 (group 2), scored 0 to 3.}
#'   \item{phq_e_2_3}{English PHQ-9 item 3, Order 2 (group 2), scored 0 to 3.}
#'   \item{phq_e_2_4}{English PHQ-9 item 4, Order 2 (group 2), scored 0 to 3.}
#'   \item{phq_e_2_5}{English PHQ-9 item 5, Order 2 (group 2), scored 0 to 3.}
#'   \item{phq_e_2_6}{English PHQ-9 item 6, Order 2 (group 2), scored 0 to 3.}
#'   \item{phq_e_2_7}{English PHQ-9 item 7, Order 2 (group 2), scored 0 to 3.}
#'   \item{phq_e_2_8}{English PHQ-9 item 8, Order 2 (group 2), scored 0 to 3.}
#'   \item{phq_e_2_9}{English PHQ-9 item 9, Order 2 (group 2), scored 0 to 3.}
#'   \item{phq_g_2_1}{Gujarati PHQ-9 item 1, Order 2 (group 1), scored 0 to 3.}
#'   \item{phq_g_2_2}{Gujarati PHQ-9 item 2, Order 2 (group 1), scored 0 to 3.}
#'   \item{phq_g_2_3}{Gujarati PHQ-9 item 3, Order 2 (group 1), scored 0 to 3.}
#'   \item{phq_g_2_4}{Gujarati PHQ-9 item 4, Order 2 (group 1), scored 0 to 3.}
#'   \item{phq_g_2_5}{Gujarati PHQ-9 item 5, Order 2 (group 1), scored 0 to 3.}
#'   \item{phq_g_2_6}{Gujarati PHQ-9 item 6, Order 2 (group 1), scored 0 to 3.}
#'   \item{phq_g_2_7}{Gujarati PHQ-9 item 7, Order 2 (group 1), scored 0 to 3.}
#'   \item{phq_g_2_8}{Gujarati PHQ-9 item 8, Order 2 (group 1), scored 0 to 3.}
#'   \item{phq_g_2_9}{Gujarati PHQ-9 item 9, Order 2 (group 1), scored 0 to 3.}
#' }
#' @source Simulated dummy data for demonstrating
#'   \code{\link{merge_counterbalanced_columns}}.
"counterbalance"
