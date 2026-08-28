package com.app.meals.ui

import androidx.compose.ui.test.*
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.test.ext.junit.runners.AndroidJUnit4
import com.app.meals.model.Meal
import com.app.meals.ui.components.ExpandableMealCard
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith

@RunWith(AndroidJUnit4::class)
class GlobalAccessibilityTest {

    @get:Rule
    val composeTestRule = createComposeRule()

    @Test
    fun `interactive elements have proper accessibility roles`() {
        val mockMeal = Meal(
            id = "1", name = "Asado", ingredients = emptyList(), measures = emptyList(),
            category = "Beef", country = "Argentina"
        )

        composeTestRule.setContent {
            ExpandableMealCard(
                meal = mockMeal,
                onCardClick = {}
            )
        }

        composeTestRule.onNodeWithText("Asado")
            .assertExists()
            .assertHasClickAction()

        composeTestRule.onNodeWithContentDescription("Show more")
            .assertExists()
            .assertHasClickAction()
    }
}