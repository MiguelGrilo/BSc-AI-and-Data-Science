package com.app.meals.ui

import androidx.compose.ui.test.assertHasClickAction
import androidx.compose.ui.test.assertIsDisplayed
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.onNodeWithContentDescription
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.performClick
import androidx.test.ext.junit.runners.AndroidJUnit4
import com.app.meals.model.Meal
import com.app.meals.ui.components.ExpandableMealCard
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith

@RunWith(AndroidJUnit4::class)
class MealCardAccessibilityTest {

    @get:Rule
    val composeTestRule = createComposeRule()

    @Test
    fun `expandable meal card toggles content and semantic descriptions for accessibility`() {
        val mockMeal = Meal(
            id = "1", name = "Tacos", ingredients = listOf("Carne", "Milho"), measures = listOf("1", "2"),
            category = "Mexicana", country = "México"
        )

        composeTestRule.setContent {
            ExpandableMealCard(meal = mockMeal, onCardClick = {})
        }

        composeTestRule.onNodeWithContentDescription("Tacos").assertIsDisplayed()

        composeTestRule.onNodeWithContentDescription("Show more")
            .assertIsDisplayed()
            .assertHasClickAction()
            .performClick()

        composeTestRule.onNodeWithText("Requires 2 ingredients. Tap to view full recipe.").assertIsDisplayed()

        composeTestRule.onNodeWithContentDescription("Show less").assertIsDisplayed()
    }
}