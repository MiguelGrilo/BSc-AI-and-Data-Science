package com.app.meals.ui

import androidx.compose.ui.test.assertIsDisplayed
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.onNodeWithContentDescription
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.performClick
import androidx.test.ext.junit.runners.AndroidJUnit4
import com.app.meals.model.Meal
import com.app.meals.ui.screens.HomeScreen
import com.app.meals.ui.viewModels.HomeUiState
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith

@RunWith(AndroidJUnit4::class)
class HomeScreenTest {

    @get:Rule
    val composeTestRule = createComposeRule()

    @Test
    fun `displays error message and retry button when state is Error`() {
        composeTestRule.setContent {
            HomeScreen(
                homeUiState = HomeUiState.Error,
                onRefresh = {},
                retryAction = {},
                onMealClick = {}
            )
        }

        composeTestRule.onNodeWithText("Failed to load meals..").assertIsDisplayed()
        composeTestRule.onNodeWithText("Retry").assertIsDisplayed()
    }

    @Test
    fun `displays grid of meals when state is Success`() {
        val mockMeal = Meal(
            id = "53133",
            name = "Asado",
            ingredients = listOf("Mixed Beef Cuts", "Chorizo", "Morcilla", "Salt"),
            measures = listOf("2kg", "4", "2", "To taste"),
            category = "Beef",
            country = "Argentina"
        )

        composeTestRule.setContent {
            HomeScreen(
                homeUiState = HomeUiState.Success(listOf(mockMeal)),
                onRefresh = {},
                retryAction = {},
                onMealClick = {}
            )
        }
        composeTestRule.onNodeWithText("Asado").assertIsDisplayed()

        composeTestRule.onNodeWithContentDescription("Show more").performClick()

        composeTestRule.onNodeWithText("Beef", substring = true, ignoreCase = true).assertIsDisplayed()
        composeTestRule.onNodeWithText("Argentina", substring = true, ignoreCase = true).assertIsDisplayed()
    }
}