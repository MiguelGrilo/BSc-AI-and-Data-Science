package com.app.meals.data.mapper

import com.app.meals.data.dto.MealDTO
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class MealMapperTest {

    @Test
    fun `toDomain clears null empty and spaced strings`() {
        val dto = MealDTO(
            id = "1",
            name = "Teste",
            imageUrl = "url",
            ingredient1 = " Frango ",
            ingredient2 = "  ",
            ingredient3 = null,
            measure1 = " 1kg ",
            measure2 = "",
            measure3 = null
        )
        val meal = dto.toDomain()

        assertEquals(1, meal.ingredients.size)
        assertEquals("Frango", meal.ingredients.first())
        assertEquals(1, meal.measures.size)
        assertEquals("1kg", meal.measures.first())
    }

    @Test
    fun `toDomain works with completely empty lists`() {
        val dto = MealDTO(id = "2", name = "Água", imageUrl = "url")
        val meal = dto.toDomain()

        assertTrue(meal.ingredients.isEmpty())
        assertTrue(meal.measures.isEmpty())
    }
}